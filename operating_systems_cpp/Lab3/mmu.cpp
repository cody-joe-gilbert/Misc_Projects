/*Lab 3: Memory Management
  Cody Gilbert
  Operation Systems*/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <list>
#include <iomanip>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>



/* Bit hacks header operations taken from  http://www.catonmat.net/blog/bit-hacks-header-file/ */
#define HEXIFY(X) 0x##X##LU

#define B8IFY(Y) (((Y&0x0000000FLU)?1:0)  + \
                  ((Y&0x000000F0LU)?2:0)  + \
                  ((Y&0x00000F00LU)?4:0)  + \
                  ((Y&0x0000F000LU)?8:0)  + \
                  ((Y&0x000F0000LU)?16:0) + \
                  ((Y&0x00F00000LU)?32:0) + \
                  ((Y&0x0F000000LU)?64:0) + \
                  ((Y&0xF0000000LU)?128:0))

#define B8(Z) ((unsigned char)B8IFY(HEXIFY(Z)))

  /* set n-th bit in x */
#define B_SET(x, n)      ((x) |= (1<<(n)))

#define B_TOGGLE(x, n)   ((x) ^= (1<<(n)))

using namespace std;

// Global Variables
bool verbose = false;  // Debugging flag
const int PTENumber = 64; // Max Number of virtual Page Table Entries
const int ESCNRUResetFreq = 50; // Reset frequency for the ESCNRU algo
const int NumESCNRUClasses = 4; // Number of ESCNRU classes
const int tau = 50; // WSClock Replacement Algo Time Delta
int MaxFrameNum = 128;  // Max number of  memory frames
int numInstr = -1; // # Number of instructions
int CxtSwitchs = 0; // Number of Context Switches
int procExits = 0; // Number of Process Exits
int RWinst = 0; // Number of Read/Write instructions
bool perinstruction = false;  // Prints each operations; corresponds to option O
bool ESCNRUVerb = false;  // Prints ASELECT output; option a
bool ProcessSummary = false; // Prints Process Summary output; option P
bool FrameSummary = false; // Prints frame designations; option F
bool FinalSummary = false; // Prints final execution summary; option S


class Frame {
public:
		int proc;
		int vpage;
		bool free;
		unsigned time : 32;

		Frame() {
			free = true;
			time = 0;
		}

};

vector<Frame*> frame_table;

class VMAc {
public:
	// Virtual memory address class
	bool hole;
	bool wo; // write-only
	bool fmap; // file-mapped
};

struct PTE {
	unsigned valid : 1;
	unsigned write_protect : 1;
	unsigned modified : 1;
	unsigned referenced : 1;
	unsigned pagedout : 1;
	unsigned frame : 7;
};

class Process {
public:
	int pid;  // Process ID
	// Statistics
	int unmaps;  // # Unmaps
	int maps;  // # Maps
	int ins;  // # Page Ins
	int outs; // # Page Outs
	int fins; // # File Page Ins
	int fouts; // # Files Page Outs
	int zeros; // # Zero Opertations
	int segv; // # segv
	int segprot; // # segprotects
	vector<PTE*> PT;
	vector<vector<int>*> VMAL; // Vitrual Memory Address from Input, NOT the VMA
	vector<VMAc*> VMA; // Virtual Memory Address list
	Process() {
		// Initialize the Page Table Entries to 0
		for (int i = 0; i < PTENumber; i++) {
			PTE* entry = new PTE;
			entry->valid = 0;
			entry->write_protect = 0;
			entry->modified = 0;
			entry->referenced = 0;
			entry->pagedout = 0;
			entry->frame = 0;
			PT.push_back(entry);
		}
		// Initialize the virtual memory addresses
		for (int i = 0; i < PTENumber; i++) {
			VMAc* entry = new VMAc;
			entry->hole = true;
			VMA.push_back(entry);
		}

	}
};

Process* current_process;
vector<Process*> PL;  // Process List

int readInt(string strNumber) {
	// Converts a string to an int
	int outInt; // output int if acceptable
	stringstream streamConverter(strNumber);
	streamConverter >> outInt;
	if (strNumber.find_first_not_of("0123456789") != string::npos) {
		// If not a number, raise error
		cout << "ERROR: " << strNumber << " not an int" << endl;
		exit(-1);
	}
	return outInt;
}

vector<Process*> readProcesses(ifstream& inputfile) {
	// Read in the processes and virtual addresses of the process
	vector<Process*> PL;
	vector<int>* vma;
	vector<int> VMALEntry;
	string line;
	string var1, var2, var3, var4;
	int numProcess;
	int numAddresses;
	int start; // counter placeholder
	int end; // counter placeholder
	// Find the number of processes, skipping comments
	while (getline(inputfile, line))
	{
		istringstream ss(line);
		ss >> var1 ;
		if (var1[0] == '#') continue;
		numProcess = readInt(var1);
		break;
	}
	// Create each process
	for (int i = 0; i < numProcess; i++) {
		Process* proc = new Process;
		proc->pid = i;
		// Find the number of addresses for each process
		while (getline(inputfile, line))
		{
			istringstream ss(line);
			ss >> var1;
			if (var1[0] == '#') continue;
			numAddresses = readInt(var1);
			break;
		}
		// read each memory address
		for (int j = 0; j < numAddresses; j++) {
			vector<int>* vma = new vector<int>;
			// Find the number of addresses for each process
			while (getline(inputfile, line))
			{
				istringstream ss(line);
				ss >> var1 >> var2 >> var3 >> var4;
				if (var1[0] == '#') continue;
				vma->push_back(readInt(var1));
				vma->push_back(readInt(var2));
				vma->push_back(readInt(var3));
				vma->push_back(readInt(var4));
				break;
			}
			proc->VMAL.push_back(vma);
		}
		// Create each process's VMA table
		for (int i = 0; i < proc->VMAL.size(); i++) {
			VMALEntry = *proc->VMAL[i];
			start = VMALEntry[0];
			end = VMALEntry[1];
			for (int j = start; j <= end; j++) {
				proc->VMA[j]->hole = false;
				proc->VMA[j]->wo = VMALEntry[2];
				proc->VMA[j]->fmap = VMALEntry[3];
			}
		}
		PL.push_back(proc);
	}
	
	if (verbose) {
		cout << "Input Process info: " << endl;
		for (int i = 0; i < PL.size(); i++) {
			cout << "pid: " << PL[i]->pid << endl;
			cout << "Number of Addresses: " << PL[i]->VMAL.size() << endl;
			for (int j = 0; j < PL[i]->VMAL.size(); j++) {
				vector<int> vma = *PL[i]->VMAL[j];
				cout << vma[0] << " ";
				cout << vma[1] << " ";
				cout << vma[2] << " ";
				cout << vma[3] << endl;
			}
			cout << "VMA Table" << endl;
			for (int j = 0; j < PL[i]->VMA.size(); j++) {
				VMAc* vma = PL[i]->VMA[j];
				cout << j << ":  " << vma->hole << " ";
				cout << "hole=" << vma->hole << " ";
				if (!vma->hole) {
					cout << "wo=" << vma->wo << " ";
					cout << "fmap=" << vma->fmap << " ";
				}
				cout  << endl;
			}
		}
		
	}
	return PL;
}

vector<vector<string>> readInstructions(ifstream& inputfile) {
	vector<vector<string>> IS;
	string line;
	string var1, var2;
	while (getline(inputfile, line))
	{
		istringstream ss(line);
		ss >> var1 >> var2;
		if (var1[0] == '#') continue;
		vector<string> instruction;
		instruction.push_back(var1);
		instruction.push_back(var2);
		IS.push_back(instruction);
	}
	if (verbose) {
		cout << "Input Instruction Set: " << endl;
		for (int i = 0; i < IS.size(); i++) {
			cout << IS[i][0] << " ";
			cout << IS[i][1] << endl;
		}
	}
	return IS;
}

bool get_next_instr(string &operation, int &vpage, vector<vector<string>> &IS) {
	if (IS.size() == 0) return false;
	operation = IS.front()[0];
	vpage = readInt(IS.front()[1]);
	IS.erase(IS.begin());
	return true;
}

class PickerBase {
public:
	list<int> frame_queue;
	list<int> free_frame_queue;
	PickerBase() {
		for (int i = 0; i < MaxFrameNum; i++) {
			Frame* frameentry = new Frame;
			frame_table.push_back(frameentry); // The entries on this list never change order (e.g. frame 0 is always the first element)
			frame_queue.push_back(i);  // This list is manipulated (e.g FIFO), with int pointers to the frame table
			free_frame_queue.push_back(i);
		}
	}
	virtual void reset_age(int FrameNum) {
		// Generic virtual function; does nothing
		// for any replacement algorithm but Aging and WSClock
		return;
	}

	virtual int get_frame() {
		int nFrame; // Chosen frame number
		// Retrieve a free frame
		// for (auto const& i : frame_queue) {
		if (free_frame_queue.size() > 0) {
			nFrame = free_frame_queue.front();
			free_frame_queue.pop_front();
			frame_table[nFrame]->free = false;
			return nFrame;
		}
		
		// if no frames are free, invoke pager
		return -1;
	}
	virtual void unmap(int victim_Frame, bool proc_exit) {
		// unmap a victim frame
		int vpid = frame_table[victim_Frame]->proc;  // Victim Process pid
		int vVpage = frame_table[victim_Frame]->vpage;  // Victim Virtual Page
		Process* vProc = PL[vpid]; // Victim process pointer
		PTE* vpte = vProc->PT[vVpage];
		vector<vector<int>*> VMAL = vProc->VMAL;
		vector<int> VMALentry;
		if (perinstruction) cout << " UNMAP " << vpid << ":" << vVpage << endl;
		vProc->unmaps++;
		vpte->valid = 0;
		if (vProc->VMA[vVpage]->fmap && vpte->modified) {
			// if it's a mapped file, 
			// then file swap out
			vProc->fouts++;
			//vpte->pagedout = 1;
			vpte->modified = 0;
			if (perinstruction) cout << " FOUT" << endl;
		}
		else if (vpte->modified) {
			// If modified, swap out to disk
			vpte->modified = 0;
			if (!proc_exit) {
				// If it's a process exit, then we don't
				// have to write swaps back to disk
				vProc->outs++;
				vpte->pagedout = 1;
				if (perinstruction) cout << " OUT" << endl;
			}
		}
		else {
			// If it was never modifed, then there's nothing to page out.
			return;
		}
	}
	virtual void free_frames() {
		// Will free all frames within the page table
		// of the current process.
		Process * proc = current_process;
		int freeFrame;
		for (int i = 0; i < proc->PT.size(); i++) {
			proc->PT[i]->pagedout = 0;
			if (proc->PT[i]->valid) {
				freeFrame = proc->PT[i]->frame;
				unmap(freeFrame, true);
				frame_table[freeFrame]->free = true;
				free_frame_queue.push_back(proc->PT[i]->frame);
			}
			else {
				// invalid frames need to be restored as well
				proc->PT[i]->modified = 0;
			}
		}
	}
};

class FIFO : public PickerBase {
	// First-In-First-Out Replacement algorithm
public:
	int pager() {
		int vFrame = frame_queue.front();
		frame_queue.pop_front();
		frame_queue.push_back(vFrame);
		unmap(vFrame, false);
		return vFrame;
	
	}
};

class Random : public PickerBase {
	// Random Replacement algorithm
public:
	ifstream rfile;
	string rfname;
	string temps;
	int rlength;
	int rfile_bookmark;

	Random(string randfile) {
		// first reads in the random file
		rfile_bookmark = 1;
		rfname = randfile;
		rfile.open(rfname);
		if (rfile.is_open()) {
			// First line of rfile is its length, so set that as max
			getline(rfile, temps);
			rlength = readInt(temps);
			rfile.close();
		}
		else {
			cout << "Cannot Open Input File " << rfname << endl;
			exit(EXIT_FAILURE);
		}
		
	}

	int getRand() {
		int rint;
		if (rfile_bookmark == rlength) {
			// exceeded size of rfile; wrap around
			rfile_bookmark = 1;
		}
		rfile.open(rfname);
		if (rfile.is_open()) {
			getline(rfile, temps); // skip the first line
			for (int i = 0; i < rfile_bookmark; i++) {
				// this is a very inefficient method of 
				// reading through the file each time a
				// random number is called, however it precludes saving in memory
				// and isn't how random numbers are usually generated.
				getline(rfile, temps);
			}
			rint = readInt(temps);
			rfile.close();
			rfile_bookmark++;
		}
		else {
			cout << "Cannot Open Input File " << rfname << endl;
			exit(EXIT_FAILURE);
		}
		
		rint = rint % MaxFrameNum;
		return rint;

	}

	int pager() {
		int vFrame = getRand();
		unmap(vFrame, false);
		return vFrame;

	}
};

class Clock : public PickerBase {
	// Clock Replacement algorithm
public:
	int pager() {
		int vFrame;
		int vProc;
		PTE* vpte;
		while (true) {
			// Here, the hand of the clock is represented by the front of
			// the frame_queue list. Each "tick" swaps pushes the front
			// element to the rear.
			vFrame = frame_queue.front();
			vProc = frame_table[vFrame]->proc;
			frame_queue.pop_front();
			frame_queue.push_back(vFrame);
			vpte = PL[vProc]->PT[frame_table[vFrame]->vpage];
			if (vpte->referenced == 0) {
				// If no reference, select as victim
				unmap(vFrame, false);
				return vFrame;
			}
			else {
				// if it has been referenced, then reset it and continue
				vpte->referenced = 0;
			}
		}

	}
};

class ESCNRU : public PickerBase {
	// ESC/NRU Replacement algorithm
public:
	int hand;
	int last_reset;
	vector<int> class_pos;

	ESCNRU() {
		hand = 0;
		last_reset = -1;
		for (int i = 0; i < NumESCNRUClasses; i++) {
			// initialize each class pointer to -1
			class_pos.push_back(-1);
		}
	}

	int pager() {
		bool resetFlag = false;
		int nScanned = 0;
		int frame_class;
		int score;
		int vFrame;
		int vProc;
		PTE* vpte;
		if (numInstr - last_reset >= ESCNRUResetFreq) {
			// Reset reference bits if the instruction limit has passed
			resetFlag = true;
			last_reset = numInstr;
		}
		for (int i = 0; i < NumESCNRUClasses; i++) {
			// initialize each class pointer to -1
			class_pos[i] = -1;
		}
		for (int i = 0; i < MaxFrameNum; i++) {
			// circularly iterate over all frames,
			// starting at the last hand 
			nScanned++;
			vFrame = (i + hand) % MaxFrameNum;
			
			vProc = frame_table[vFrame]->proc;
			vpte = PL[vProc]->PT[frame_table[vFrame]->vpage];
			score = 2 * vpte->referenced + vpte->modified;
			switch(score) {
			case 0:
				if (class_pos[score] == -1) {
					class_pos[score] = i;
				}
				if (!resetFlag) {
					// if we aren't reseting references, break and report
					// this frame as the victim.
					if (ESCNRUVerb) {
						cout << "ASELECT: ";
						cout << hand << " ";
						cout << resetFlag << " | ";
						cout << "0" << " ";
					}
					hand = (vFrame + 1) % MaxFrameNum;
					if (ESCNRUVerb) {
						cout << vFrame << " ";
						cout << nScanned << endl;
					}
					unmap(vFrame, false);
					return vFrame;
				}
				break;
			default:
				if (class_pos[score] == -1) {
					class_pos[score] = vFrame;
				}
				break;
			}
			if (resetFlag) {
				// if the reset flag is set, set back the refererenced bit for the pte
				vpte->referenced = 0;
			}
		}
		for (int i = 0; i < class_pos.size(); i++) {
			if (class_pos[i] != -1) {
				if (ESCNRUVerb) {
					cout << "ASELECT: ";
					cout << hand << " ";
					cout << resetFlag << " | ";
					cout << i << " ";
				}
				hand = (class_pos[i] + 1) % MaxFrameNum;
				if (ESCNRUVerb) {
					cout << class_pos[i] << " ";
					cout << nScanned << endl;
				}
				unmap(class_pos[i], false);
				return class_pos[i];
			}
		}
		
	}
};

class Aging : public PickerBase {
	// Aging Replacement algorithm
public:
	int hand;
	Aging() {
		hand = 0;
	}
	void reset_age(int FrameNum) {
		// Actual content for resetting the frame age
		frame_table[FrameNum]->time = 0;
	}
	int pager() {
		
		int last_reset;
		unsigned int min_time = frame_table[hand]->time;
		min_time = min_time >> 1;
		B_SET(min_time, 31);
		int min_frame = hand;
		int vFrame;
		int vProc;
		PTE* vpte;
		if (ESCNRUVerb) {
			cout << "ASELECT ";
			cout << hand << "-";
			cout << ((hand - 1) % MaxFrameNum) << " ";
		}
		for (int i = 0; i < MaxFrameNum; i++) {
			vFrame = (hand + i) % MaxFrameNum;
			vProc = frame_table[vFrame]->proc;
			vpte = PL[vProc]->PT[frame_table[vFrame]->vpage];
			// bitshift the age bitvector
			frame_table[vFrame]->time = frame_table[vFrame]->time >> 1;
			// add the ref bit to the end
			if (vpte->referenced) {
				B_SET(frame_table[vFrame]->time, 31);
				vpte->referenced = 0;
			}
			if (frame_table[vFrame]->time < min_time ) {
				min_time = frame_table[vFrame]->time;
				min_frame = vFrame;
			}
			if (ESCNRUVerb) {
				cout << vFrame << ":";
				cout << hex << frame_table[vFrame]->time << dec << " ";
			}

		}
		hand = (min_frame + 1) % MaxFrameNum;
		if (ESCNRUVerb) {
			cout << "| ";
			cout << min_frame << endl;
		}
	
		
		
		unmap(min_frame, false);
		return min_frame;

	}
};

class WorkingSet : public PickerBase {
	// Working Set Replacement algorithm
public:
	int hand;

	WorkingSet() {
		hand = 0;
	}

	void reset_age(int FrameNum) {
		// Modify reset_age to have
		// each frame contain the update time
		frame_table[FrameNum]->time = numInstr;
		return;
	}

	int pager() {
		bool resetFlag = false;
		int nScanned = 0;
		int frame_class;
		int score;
		int vFrame;
		int vProc;
		int max_age = 0;
		int age;
		int max_frame = hand;
		PTE* vpte;
		if (ESCNRUVerb) {
			cout << "ASELECT ";
			cout << hand << "-";
			cout << ((hand - 1) % MaxFrameNum) << " ";
		}
		for (int i = 0; i < MaxFrameNum; i++) {
			vFrame = (hand + i) % MaxFrameNum;
			vProc = frame_table[vFrame]->proc;
			vpte = PL[vProc]->PT[frame_table[vFrame]->vpage];
			age = numInstr - frame_table[vFrame]->time;
			
			if (ESCNRUVerb) {
				cout << vFrame << " (";
				cout << vpte->referenced << " ";
				cout << vProc << ":" << frame_table[vFrame]->vpage << " ";
				cout << frame_table[vFrame]->time << ") ";
			}
			if (vpte->referenced) {
				// if referenced within time delta,
				// reset ref bit and time
				vpte->referenced = 0;
				frame_table[vFrame]->time = numInstr;
			}
			else {
				// If the reference bit is set, look at modifed bit and age
				if (!vpte->modified && age >= tau) {
					// if it's a clean page, that hasn't been touched
					// within the interval, then choose it:
					if (ESCNRUVerb) {
						cout << " | " << vFrame << endl;
					}
					hand = (vFrame + 1) % MaxFrameNum;
					unmap(vFrame, false);
					return vFrame;
				}
				else {
					// Otherwise, note its time delta and record the max age
					if (age > max_age) {
						max_age = age;
						max_frame = vFrame;
					}
				}
			}
		}
		if (ESCNRUVerb) {
			cout << " | " << max_frame << endl;
		}
		// If no page was selected, then report the max_frame
		hand = (max_frame + 1) % MaxFrameNum;
		unmap(max_frame, false);
		return max_frame;
	}
};

bool page_fault_handler(int vpage, bool &filemapped, bool &writeProt) {
	vector<vector<int>*> VMAL = current_process->VMAL;
	vector<VMAc*> VMA = current_process->VMA;
	vector<int> VMALentry;
	// First, verify that the vpage is valid for the process
	if (VMA[vpage]->hole) {
		// if it's a hole, return a segv
		if (verbose) cout << "Invalid vpage: " << vpage << endl;
		if (perinstruction) cout << " SEGV" << endl;
		current_process->segv++;
		return false;
	}

	filemapped = VMA[vpage]->fmap;
	writeProt = VMA[vpage]->wo;
	return true;
}

template<typename R>
void Simulator(R* picker, vector<vector<string>> &IS) {
	string operation;
	int vpage;
	PTE* pte;
	Frame* wFrame; // working frame for simulation
	int framenum; // Frame number of a chosen frame
	bool filemapped; // Indicates a given VMPage is file mapped
	bool writeProt; // Indicates a given VMPage is file mapped
	while (get_next_instr(operation, vpage, IS)) {
		numInstr++; // Increment number of instructions
		if (perinstruction) {
			cout << numInstr << ": ";
			cout << "==> " << operation << " " << vpage << endl;
		}
		if (operation == "c") {
			// Perform context switch
			CxtSwitchs++;
			for (int i = 0; i < PL.size(); i++) {
				// Search for the process by pid and set to switched
				// process given by the vpage variable
				if (vpage == PL[i]->pid) current_process = PL[i];
			}
		}
		else if (operation == "e") {
			// Functionality to free frames exist, but hasn't been tested
			// since no files have that option
			if (perinstruction) cout << "EXIT current process " << vpage << endl;
			procExits++;
			picker->free_frames();
			continue;
		}
		else if (operation == "r" || operation == "w") {
			// Read or write instruction
			RWinst++;
			pte = current_process->PT[vpage];
			if (!pte->valid) {
				// If the PT entry is invalid, create page fault exception
				if (!page_fault_handler(vpage, filemapped, writeProt)) {
					// page_fault_handler determines validity and file mapping status
					continue;  // if it wasn't valid, continue to next operation
				}
				// If the vpage is valid, then we need to swap it in, and map to a frame
				framenum = picker->get_frame();
					if (framenum < 0) {
						framenum = picker->pager();
					}
				wFrame = frame_table[framenum];
				wFrame->proc = current_process->pid;
				wFrame->vpage = vpage;
				pte->write_protect = writeProt;
				//if (current_process->VMA[vpage]->fmap && !pte->pagedout) {
				if (current_process->VMA[vpage]->fmap) {
					// Clean file mapped file; retrieve from file
					current_process->fins++;
					if (perinstruction) cout << " FIN" << endl;
				}
				else if (!current_process->VMA[vpage]->fmap && !pte->pagedout) {
					// Clean file not file mapped; return zeros
					current_process->zeros++;
					if (perinstruction) cout << " ZERO" << endl;
				}
				else {
					// Was previously swapped to disk; swap back in
					current_process->ins++;
					if (perinstruction) cout << " IN" << endl;
				}
				// Set map and flag
				current_process->maps++;
				pte->valid = 1;
				pte->modified = 0;
				pte->frame = framenum;
				picker->reset_age(framenum);
				if (perinstruction) {
					cout << " MAP " << framenum << endl;
				}
			}
			if (operation == "r") {
				// Perform a read operation
				pte->referenced = 1;
			}
			else {
				// Perform a write operation
				pte->referenced = 1;
				if (pte->write_protect) {
					// Attempting to write to a write protected file
					// raise seg prot fault and continue
					current_process->segprot++;
					if (verbose) cout << "SEG PROT" << endl;
					if (perinstruction) cout << "  SEGPROT" << endl;
					continue;
				}
				pte->modified = 1;
			}
		}
		else {
			cout << "Invalid Operation: " << operation << endl;
			exit(-1);
		}
	}
}

int main(int argc, char **argv)
{
	// Initialize variables
	enum Replacements { iFIFO, iRand, iClock, iESCNRU, iAging, iWorkingSet };
	int repType;
	vector<vector<string>> IS;
	string inputFile;
	string randFile;

	// Get options
	stringstream ss;
	string s; // Placeholder string for input variables

	int index;
	int c;
	opterr = 0;
	while ((c = getopt(argc, argv, "a:o:f:")) != -1) {
		switch (c)
		{
		case 'a':
			ss << optarg;
			ss >> s;
			if (s == "f") {
				repType = iFIFO;
			}
			else if (s == "r") {
				repType = iRand;
			}
			else if (s == "c") {
				repType = iClock;
			}
			else if (s == "e") {
				repType = iESCNRU;
			}
			else if (s == "a") {
				repType = iAging;
			}
			else if (s == "w") {
				repType = iWorkingSet;
			}
			else {
				cout << "Error: " << s << " not a valid replacement algorithm" << endl;
				exit(1);
			}
			ss.str("");
			ss.clear();
			break;
		case 'f':
			ss << optarg;
			ss >> s;
			MaxFrameNum = readInt(s);
			ss.str("");
			ss.clear();
			break;
		case 'o':
			ss << optarg;
			ss >> s;
			if (s.find("O") != string::npos) perinstruction = true;
			if (s.find("P") != string::npos) ProcessSummary = true;
			if (s.find("F") != string::npos) FrameSummary = true;
			if (s.find("S") != string::npos) FinalSummary = true;
			if (s.find("a") != string::npos) ESCNRUVerb = true;
			ss.str("");
			ss.clear();
			break;
		case '?':
			if (optopt == 'a')
				fprintf(stderr, "Option -%c requires an argument.\n", optopt);
			else if (isprint(optopt))
				fprintf(stderr, "Unknown option `-%c'.\n", optopt);
			else
				fprintf(stderr,
					"Unknown option character `\\x%x'.\n",
					optopt);
			return 1;
		default:
			abort();
		}
	}
		// read in input file
	inputFile = argv[optind];
	// read in random file
	randFile = argv[optind + 1];
	
	// Read in the input file
	ifstream myfile(inputFile);
	if (myfile.is_open())
	{
		// Read in processes
		PL = readProcesses(myfile);
		// Read in Instructions
		IS = readInstructions(myfile);
		myfile.close();
	}
	else {
		cout << "Unable to open file";
		exit(-1);
	}
	// Initialize Frame table:

	// Determine the pager based on replacement algorithm
	switch (repType) {
	case iFIFO :
		{FIFO* picker = new FIFO; 
		 Simulator<FIFO>(picker, IS); }
		break;
	case iRand:
		{Random* picker= new Random(randFile);
		Simulator<Random>(picker, IS); }
		break;
	case iClock:
		{Clock* picker = new Clock;
		Simulator<Clock>(picker, IS); }
		break;
	case iESCNRU:
		{ESCNRU* picker = new ESCNRU;
		Simulator<ESCNRU>(picker, IS); }
		break;
	case iAging:
		{Aging* picker = new Aging;
		Simulator<Aging>(picker, IS); }
		break;
	case iWorkingSet:
		{WorkingSet* picker = new WorkingSet;
		Simulator<WorkingSet>(picker, IS); }
		break;
	default :
		cout << "Not a valid replacement algo" << endl;
		exit(-1);

	}

	Process* proc;
	PTE* pte;
	// Print Process summaries
	if (ProcessSummary) {
		// iterate over processes and PTEs
		for (int i = 0; i < PL.size(); i++) {
			proc = PL[i];
			cout << "PT[" << i << "]:";
			for (int j = 0; j < proc->PT.size(); j++) {
				pte = proc->PT[j];
				if (pte->valid) {
					cout << " " << j << ":";
					if (pte->referenced) cout << "R";
					else cout << "-";
					if (pte->modified) cout << "M";
					else cout << "-";
					if (pte->pagedout) cout << "S";
					else cout << "-";
				}
				else if (!pte->valid && pte->pagedout) {
					cout << " #";
				}
				else {
					cout << " *";
				}
			}
			cout << endl;
		}
	}
	Frame* frame;
	if (FrameSummary) {
		cout << "FT: ";
		for (int i = 0; i < frame_table.size(); i++) {
			frame = frame_table[i];
			if (frame->free) {
				cout << "* ";
			}
			else {
				cout << frame->proc << ":" << frame->vpage << " ";
			}
		}
		cout << endl;
	}
	// Per Process summary
	if (ProcessSummary) {
		for (int i = 0; i < PL.size(); i++) {
				proc = PL[i];
				cout << "PROC[" << i << "]: ";
				cout << "U=" << proc->unmaps << " ";
				cout << "M=" << proc->maps << " ";
				cout << "I=" << proc->ins << " ";
				cout << "O=" << proc->outs << " ";
				cout << "FI=" << proc->fins << " ";
				cout << "FO=" << proc->fouts << " ";
				cout << "Z=" << proc->zeros << " ";
				cout << "SV=" << proc->segv << " ";
				cout << "SP=" << proc->segprot << " ";
				cout << endl;
		}
	}
	
	// Final summary
	if (FinalSummary) {
		cout << "TOTALCOST ";
		cout << numInstr + 1 << " ";
		cout << CxtSwitchs << " ";
		cout << procExits << " ";
		unsigned long long cost = 0;
		for (int i = 0; i < PL.size(); i++) {
			proc = PL[i];
			cost += proc->unmaps * 400;
			cost += proc->maps * 400;
			cost += proc->ins * 3000;
			cost += proc->outs * 3000;
			cost += proc->fins * 2500;
			cost += proc->fouts * 2500;
			cost += proc->zeros * 150;
			cost += proc->segv * 240;
			cost += proc->segprot * 300;
		}
		cost += RWinst;
		cost += CxtSwitchs * 121;
		cost += procExits * 175;
		cout << cost << endl;
	}
	

}

