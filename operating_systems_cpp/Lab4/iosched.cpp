// IOQueueLab.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

/*
Operating Systems Lab 4: IO Scheduler
Cody Gilbert
12/11/2018

*/


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
#include <queue>

using namespace std;

// Global Variables
bool verbose = false;  // Debugging flag
int track_pos = 0; // track position
int direction = 1;  // read head direction
int tot_move = 0; // Total track movement
int t = 0; // Time counter
int final_time = 0;

class Instruction {
public:
	int tstep;  // Timestamp of arival
	int track;  // Disk track
	int ID;   // request ID
	int stime; // start time
	int etime; // end time
	int rtime; // remaining time

};
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

queue<Instruction*> readInstructions(ifstream& inputfile) {
	queue<Instruction*> IS;
	string line;
	string var1, var2;
	Instruction* temp;
	int i = 0;
	while (getline(inputfile, line))
	{
		istringstream ss(line);
		ss >> var1 >> var2;
		if (var1[0] == '#') continue;
		Instruction * inst = new Instruction;
		inst->ID = i; i++;
		inst->tstep = readInt(var1);
		inst->track = readInt(var2);
		IS.push(inst);
	}
	if (verbose) {
		cout << "Instructions Echo:" << endl;
		for (int i = 0; i < IS.size(); i++) {
			temp = IS.front();
			IS.pop();
			IS.push(temp);
			cout << temp->ID << " ";
			cout << temp->tstep << " ";
			cout << temp->track << endl;
		}
	}
	return IS;
}

class QueuerBase {
public:
	queue<Instruction*> readyQueueObj;
	queue<Instruction*> AddQueueObj;
	queue<Instruction*>* readyQueue;
	queue<Instruction*>* AddQueue;

	QueuerBase() {
		readyQueue = &readyQueueObj;
		AddQueue = &AddQueueObj;
	}

	virtual void enqueue(Instruction* ins) {
		readyQueue->push(ins);
		return;
	}
	virtual Instruction* get_request() {
		return NULL;
	}

};

class FIFO : public QueuerBase {
public:
	Instruction* get_request() {
		Instruction* ret_ins;
		if (readyQueue->size() == 0) {
			return NULL;
		}
		ret_ins = readyQueue->front();
		readyQueue->pop();
		return ret_ins;
	}
};

class SSFT : public QueuerBase {
public:
	Instruction* get_request() {
		Instruction* ret_ins = NULL;
		Instruction* temp;
		int min_dist = 99999;
		int dist;
		if (readyQueue->size() == 0) {
			return NULL;
		}
		if (verbose) {
			for (int i = 0; i < readyQueue->size(); i++) {
				dist = readyQueue->front()->track - track_pos;
				cout << readyQueue->front()->ID << ":" << abs(dist) << " ";
				temp = readyQueue->front();
				readyQueue->pop();
				readyQueue->push(temp);
			}
			cout << endl;
		}
		// loop through queue to find minimum distance
		for (int i = 0; i < readyQueue->size(); i++) {
			dist = abs(track_pos - readyQueue->front()->track);
			if (dist <= min_dist) {
				if (ret_ins && dist == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
					// ensure that in the case of tie, take the element that arrived first
					temp = readyQueue->front();
					readyQueue->pop();
					readyQueue->push(temp);
					continue;
				}
				min_dist = dist;
				ret_ins = readyQueue->front();
			}
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);
		}
		// shuffle until the return request is back at the top of queue
		while (ret_ins != readyQueue->front()) {
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);
			
		}
		readyQueue->pop();
		return ret_ins;
	}
};

class LOOK : public QueuerBase {
public:
	Instruction* get_request() {
		Instruction* ret_ins = NULL;
		Instruction* temp;
		int min_dist = 99999;
		int dist;
		bool rewind = true;
		if (readyQueue->size() == 0) {
			return NULL;
		}
		// loop through queue to find minimum distance in one direction
		for (int i = 0; i < readyQueue->size(); i++) {
			dist = readyQueue->front()->track - track_pos;
			if (abs(dist) <= min_dist && dist*direction >= 0) {
				if (ret_ins && abs(dist) == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
					// ensure that in the case of tie, take the element that arrived first
					temp = readyQueue->front();
					readyQueue->pop();
					readyQueue->push(temp);
					continue;
				}
				min_dist = abs(dist);
				ret_ins = readyQueue->front();
				rewind = false; // Found an element, don't have to rewind
			}
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);
		}
		// if an request wasn't found in one direction, reverse and go the other way
		if (rewind) {
			direction = direction * -1;
			for (int i = 0; i < readyQueue->size(); i++) {
				dist = readyQueue->front()->track - track_pos;
				if (abs(dist) <= min_dist && dist*direction >= 0) {
					if (ret_ins && abs(dist) == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
						// ensure that in the case of tie, take the element that arrived first
						temp = readyQueue->front();
						readyQueue->pop();
						readyQueue->push(temp);
						continue;
					}
					min_dist = abs(dist);
					ret_ins = readyQueue->front();
				}
				temp = readyQueue->front();
				readyQueue->pop();
				readyQueue->push(temp);
			}
		}
		// shuffle until the return request is back at the top of queue
		while (ret_ins != readyQueue->front()) {
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);

		}
		readyQueue->pop();
		return ret_ins;
	}
};

class CLOOK : public QueuerBase {
public:
	Instruction* get_request() {
		Instruction* ret_ins = NULL;
		Instruction* temp;
		int min_dist = 99999;
		int dist;
		bool rewind = true;
		if (readyQueue->size() == 0) {
			return NULL;
		}
		// loop through queue to find minimum distance in one direction
		if (verbose) {
			for (int i = 0; i < readyQueue->size(); i++) {
				dist = readyQueue->front()->track - track_pos;
				cout << readyQueue->front()->ID << ":" << dist << " ";
				temp = readyQueue->front();
				readyQueue->pop();
				readyQueue->push(temp);
			}
			cout << endl;
		}
		for (int i = 0; i < readyQueue->size(); i++) {
			dist = readyQueue->front()->track - track_pos;
			if (dist <= min_dist && dist >= 0) {
				if (ret_ins && abs(dist) == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
					// ensure that in the case of tie, take the element that arrived first
					temp = readyQueue->front();
					readyQueue->pop();
					readyQueue->push(temp);
					continue;
				}
				min_dist = abs(dist);
				ret_ins = readyQueue->front();
				rewind = false; // Found an element, don't have to rewind
			}
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);
		}
		// if an request wasn't found in one direction, Then start over from begining
		
		if (rewind) {
			for (int i = 0; i < readyQueue->size(); i++) {
				dist = readyQueue->front()->track - 0;  // using 0 as base this time
				if (abs(dist) <= min_dist) {
					if (ret_ins && abs(dist) == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
						// ensure that in the case of tie, take the element that arrived first
						temp = readyQueue->front();
						readyQueue->pop();
						readyQueue->push(temp);
						continue;
					}
					min_dist = abs(dist);
					ret_ins = readyQueue->front();
				}
				temp = readyQueue->front();
				readyQueue->pop();
				readyQueue->push(temp);
			}
		}
		// shuffle until the return request is back at the top of queue
		while (ret_ins != readyQueue->front()) {
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);

		}
		readyQueue->pop();
		return ret_ins;
	}
};


class FLOOK : public QueuerBase {
public:


	void enqueue(Instruction* ins) {
		// Instead of added to readyqueue, add to addqueue
		AddQueue->push(ins);
		return;
	}
	Instruction* get_request() {
		queue<Instruction*>* TempQueue;
		Instruction* ret_ins = NULL;
		Instruction* temp;
		int min_dist = 99999;
		int dist;
		bool rewind = true;
		// Essentially CLOOK, but with a trigger to switch queues
		if (readyQueue->empty()) {
			if (verbose) {
				cout << "Switching Queues" << endl;
			}
			TempQueue = readyQueue;
			readyQueue = AddQueue;
			AddQueue = TempQueue;
		}
		if (readyQueue->empty()) {
			return NULL;
		}
		// loop through queue to find minimum distance in one direction
		for (int i = 0; i < readyQueue->size(); i++) {
			dist = readyQueue->front()->track - track_pos;
			if (abs(dist) <= min_dist && dist*direction >= 0) {
				if (ret_ins && abs(dist) == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
					// ensure that in the case of tie, take the element that arrived first
					temp = readyQueue->front();
					readyQueue->pop();
					readyQueue->push(temp);
					continue;
				}
				min_dist = abs(dist);
				ret_ins = readyQueue->front();
				rewind = false; // Found an element, don't have to rewind
			}
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);
		}
		// if an request wasn't found in one direction, reverse and go the other way
		if (rewind) {
			direction = direction * -1;
			for (int i = 0; i < readyQueue->size(); i++) {
				dist = readyQueue->front()->track - track_pos;
				if (abs(dist) <= min_dist && dist*direction >= 0) {
					if (ret_ins && abs(dist) == min_dist && readyQueue->front()->tstep > ret_ins->tstep) {
						// ensure that in the case of tie, take the element that arrived first
						temp = readyQueue->front();
						readyQueue->pop();
						readyQueue->push(temp);
						continue;
					}
					min_dist = abs(dist);
					ret_ins = readyQueue->front();
				}
				temp = readyQueue->front();
				readyQueue->pop();
				readyQueue->push(temp);
			}
		}
		// shuffle until the return request is back at the top of queue
		while (ret_ins != readyQueue->front()) {
			temp = readyQueue->front();
			readyQueue->pop();
			readyQueue->push(temp);

		}
		readyQueue->pop();


		return ret_ins;
	}
};


void Simulator(QueuerBase* queuer, queue<Instruction*> IS, list<Instruction*> &DoneQ) {
	Instruction* current_request = NULL;
	

	while (IS.size() > 0 || current_request != NULL || !queuer->readyQueue->empty() || !queuer->AddQueue->empty()) {

		// check if a new request arrived
		if (IS.size() > 0 && IS.front()->tstep == t) {
			if (verbose) {
				cout << t << ":\t";
				cout << IS.front()->ID << " add ";
				cout << IS.front()->track << endl;
			}
			queuer->enqueue(IS.front());
			IS.pop();
		}

		// if a request active and ends at this timestep
		if (current_request && current_request->rtime - 1 <= 0) {
			track_pos += direction; // Rotate once more to final pos
			tot_move++;
			current_request->etime = t;
			if (verbose) {
				cout << t << ":\t";
				cout << current_request->ID << " finish ";
				cout << current_request->etime - current_request->tstep << endl;
			}
			DoneQ.push_back(current_request);
			current_request = NULL;
		}

		// if a request active and not finished, progress
		if (current_request) {
			current_request->rtime--;
			track_pos += direction;
			tot_move++;
		}
		// if there's no current process, then fetch one
		if (!current_request) {
			current_request = queuer->get_request();
			if (current_request) {
				current_request->stime = t;
				if (current_request->track == track_pos) {
					// if the requested track is already the current track, then go ahead and wrap
					current_request->etime = t;
					if (verbose) {
						cout << t << ":\t";
						cout << current_request->ID << " finish ";
						cout << current_request->etime - current_request->tstep << endl;
					}
					DoneQ.push_back(current_request);
					current_request = NULL;
					continue;
				}
				else {
					if (current_request->track < track_pos) direction = -1; 
					else direction = 1;
					current_request->rtime = abs(current_request->track - track_pos);
				}
				if (verbose && current_request) {
					cout << t << ":\t";
					cout << current_request->ID << " issue ";
					cout << current_request->track << " ";
					cout << track_pos << endl;
				}
			}
		}
		// increment time
		t++;
	}

	return;
}

int main(int argc, char **argv) {
	// Initialize variables
	enum Queuer { iFIFO, iSSFT, iLOOK, iCLOOK, iFLOOK };
	int repType = iFLOOK;
	queue<Instruction*> IS; // contains all input requests
	list<Instruction*> DoneQ; // Contains all finished requests
	string inputFile = "C:\\Users\\Cody Gilbert\\Desktop\\Operating Systems\\Lab4\\IOQueueLab\\TestData\\input1";

	// Get options
	stringstream ss;
	string s; // Placeholder string for input variables

	int index;
	int c;
	opterr = 0;

	while ((c = getopt(argc, argv, "vs:")) != -1) {
		switch (c)
		{
		case 'v':
			verbose = true;
			break;
		case 's':
			ss << optarg;
			ss >> s;
			if (s.substr(0, 1) == "i") {
				repType = iFIFO;
			}
			else if (s.substr(0, 1) == "j") {
				repType = iSSFT;
			}
			else if (s.substr(0, 1) == "s") {
				repType = iLOOK;
			}
			else if (s.substr(0, 1) == "c") {
				repType = iCLOOK;
			}
			else if (s.substr(0, 1) == "f") {
				repType = iFLOOK;
			}
			else {
				cout << "Error: " << s.substr(0, 1) << " not a valid scheduler" << endl;
				exit(1);
			}
			break;
		case '?':
			if (optopt == 's')
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
	// Read in the input file
	ifstream myfile(inputFile);
	if (myfile.is_open())
	{
		// Read in Instructions
		IS = readInstructions(myfile);
		myfile.close();
	}
	else {
		cout << "Unable to open file";
		exit(-1);
	}

	// Determine the pager based on replacement algorithm
	switch (repType) {
	case iFIFO:
		{FIFO* queuer = new FIFO;
		Simulator(queuer, IS, DoneQ); }
		break;
	case iSSFT:
		{SSFT* queuer = new SSFT;
		Simulator(queuer, IS, DoneQ); }
		break;
	case iLOOK:
		{LOOK* queuer = new LOOK;
		Simulator(queuer, IS, DoneQ); }
		break;
	case iCLOOK:
		{CLOOK* queuer = new CLOOK;
		Simulator(queuer, IS, DoneQ); }
		break;
	case iFLOOK:
		{FLOOK* queuer = new FLOOK;
		Simulator(queuer, IS, DoneQ); }
		break;
	default:
		cout << "Not a valid queuing algo" << endl;
		exit(-1);
	}

	// Print request results
	Instruction* temp;
	int i = 0;
	double avg_wait = 0; // average wait time
	double avg_turnaround = 0; // average turnaround time
	int max_wait = -1;
	while (DoneQ.size() > 0) {
		temp = DoneQ.front();
		DoneQ.pop_front();
		if (temp->ID == i) {
			cout << "\t" << temp->ID << ":\t";
			cout << "\t" << temp->tstep << " ";
			cout << "\t" << temp->stime << " ";
			cout << "\t" << temp->etime << endl;
			i++;
			avg_wait += temp->stime - temp->tstep;
			avg_turnaround += temp->etime - temp->tstep;
			if (temp->etime > final_time) final_time = temp->etime;
			if (temp->stime - temp->tstep > max_wait) max_wait = temp->stime - temp->tstep;
		}
		else {
			DoneQ.push_back(temp);
		}
	}
	// print summary stats
	cout << "SUM:" << " ";
	cout << final_time << " ";
	cout << tot_move << " ";
	printf("%.2f ", avg_turnaround / i);
	printf("%.2f ", avg_wait / i);
	cout << max_wait << endl;

}



