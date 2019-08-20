/*Lab 2: Scheduler 
  Cody Gilbert
  Operation Systems*/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <queue>
#include <iomanip>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
using namespace std;

// Initialize Global Variables
int CURRENT_TIME = 0;
int IOCount = 0; // Number of processes in IO Burst
int IODownTS = 0; // Timestamp of tracking IO downtime
int IODownTime = 0; // Time that 0 processes are in IO burst
int RunCount = 0;  // Number of processes running at a given time
int RunDownTS = 0;  // Timestamp of tracking Run downtime
int RunDownTime = 0;  // Time that 0 processes are running
int TotalTime = 0;  // Final total runtime (updated as processes terminate)
const int NUMPRIOS = 4; // Number of priority levels; assumed to be 4
bool verbose = false;

class Process {
public:
	enum State {CREATE, READY, RUNNING, BLOCKED, PREMPT, TERMINATED};
	int pid; // Process ID
	State Status;
	State oldStatus;
	int AT;  // Arival time
	int TC;  // Total CPU Time
	int aTC;  // Arrival Total CPU Time (Static)
	int CB;  // CPU Burst
	int IO;  // IO Burst
	int stateTC;  // Time in current state
	int stateIB; // State IO Burst
	int stateCB; // State CB Burst
	int prevStateTC; // Time in previous state
	int staticPriority;
	int dynamicPriority;
	int FT; // Finishing Time
	int TT; // Turnaround Time
	int IT; // I/O Time
	int CW; // CPU Waiting Time
	int PremptTime; // Time under which the process was prempted
	bool Prempted; // Flag stating process was preempted, instead of IB
	bool PriorityReset; // For Prio schedulers, requests the process be put on the expired queue

	Process() {
		Status = CREATE;
		oldStatus = CREATE;
		stateTC = 0;
		CW = 0;
		IT = 0;
		Prempted = false;
		PriorityReset = false;
		IODownTS = 0;
		IOCount = 0;
	}
	string StateToString(State st) {
		// Method to conver enum states to strings
		switch (st) {
		case Process::CREATE:
			return "CREATE";
		case Process::READY:
			return "READY";
		case Process::RUNNING:
			return "RUNNING";
		case Process::BLOCKED:
			return "BLOCKED";
		case Process::PREMPT:
			return "PREMPTED";
		case Process::TERMINATED:
			return "TERMINATED";
		default:
			return "ER:UNDEF";
		}
	}
	void setStatus(State stat) {
		// Switch Status and perform accounting
		oldStatus = Status;
		Status = stat;

		// Track run downtown via process counters
		if (Status == Process::RUNNING) {
			if (RunCount == 0) {
				// If no processes were running, add the time delta
				// to the running downtime total
				RunDownTime += CURRENT_TIME - RunDownTS;
			}
			RunCount++;
			if (RunCount > 1) {
				// Error flag for checking non-concurrency
				cout << "Error: No multithreading allowed" << endl;
				exit(-1);
			}
		}
		if (oldStatus == Process::RUNNING) {
			RunCount--;
			if (RunCount == 0) {
				// If the last process finished, mark the time
				RunDownTS = CURRENT_TIME;
			}
		}

		// Track IO downtown via process counters
		if (Status == Process::BLOCKED) {
			// If no processes are in IO, add the time delta
			// to the IO downtime total
			if (IOCount == 0) {
				IODownTime += CURRENT_TIME - IODownTS;
			}
			IOCount++;
		} 
		if (oldStatus == Process::BLOCKED) { 
			IOCount--;
			// If the last process finished IO, mark the time
			if (IOCount == 0) {
				IODownTS = CURRENT_TIME;
			}
		}

		// Final accounting if a process terminates
		if (Status == Process::TERMINATED) {
			FT = CURRENT_TIME;
			TT = FT - AT;
			TotalTime = CURRENT_TIME;
		}

		// Process-level Accounting for CPU and IO wait time. 
		if (oldStatus == Process::READY) CW += prevStateTC;
		if (oldStatus == Process::BLOCKED) IT += prevStateTC;

		// Print Process Transition output info if verbose
		if (verbose) {
			cout << "t=" << stateTC << " ";
			cout << "pid=" << pid << " ";
			cout << "StateTime=" << prevStateTC << " ";
			cout << StateToString(oldStatus) << "->";
			cout << StateToString(Status) << " ";
			cout << "CB=" << stateCB << " ";
		
			if (oldStatus == Process::RUNNING && Status == Process::BLOCKED) {
				cout << "IB=" << stateIB << " ";
				cout << "REM=" << TC << " ";
			}
			if (oldStatus == Process::READY && Status == Process::RUNNING) {
				cout << "CB=" << stateCB << " ";
				cout << "REM=" << TC << " ";
				cout << "PRIO=" << dynamicPriority << " ";
			}
			if (oldStatus == Process::BLOCKED && Status == Process::READY) {
				cout << "CB=" << stateCB << " ";
				cout << "REM=" << TC << " ";
				cout << "PRIO=" << dynamicPriority << " ";
			}
			cout << endl;
		}
	}
};

class Event {
public:
	enum Transit {TRANS_TO_READY, TRANS_TO_RUN, TRANS_TO_BLOCK, TRANS_TO_PREEMPT, TRANS_TO_TERM};
	int timestamp;
	Process* process;
	Transit transition;

	string transitionToString(Transit trans) {
		switch (trans) {
		case Event::TRANS_TO_READY:
			return "TRANS_TO_READY";
		case Event::TRANS_TO_RUN:
			return "TRANS_TO_RUN";
		case Event::TRANS_TO_BLOCK:
			return "TRANS_TO_BLOCK";
		case Event::TRANS_TO_PREEMPT:
			return "TRANS_TO_PREEMPT";
		case Event::TRANS_TO_TERM:
			return "TRANS_TO_TERM";
		default:
			return "ER:UNDEF";
		}
	}
};

class Token {
public:
	string token;
	int lineNum;
	int offsetNum;
};

void parseerror(int errcode, Token* errTok) {
	string errstr[] = {
		"NUM_EXPECTED",
		"SYM_EXPECTED",
		"ADDR_EXPECTED",
		"SYM_TOO_LONG",
		"TOO_MANY_DEF_IN_MODULE",
		"TOO_MANY_USE_IN_MODULE",
		"TOO_MANY_INSTR",
	};
	cout << "Parse Error line " << errTok->lineNum;
	cout << " offset " << errTok->offsetNum;
	cout << ": " << errstr[errcode] << endl;
	exit(EXIT_FAILURE);
}

int readInt(Token* token) {
	string strNumber = token->token;
	int outInt; // output int if acceptable
	if (strNumber.find_first_not_of("0123456789") != string::npos) {
		// If not a number, raise NUM_EXPECTED
		parseerror(0, token);
	}
	stringstream streamConverter(strNumber);
	streamConverter >> outInt;

	return outInt;
}

template<typename T>
class ListNode {
public:
	T data;
	ListNode* next;
	ListNode* prev;
};

template<typename T>
class GeneralList {
public:
	ListNode<T>* head;
	ListNode<T>* tail;
	ListNode<T>* cur;
	int length;
	bool circular; // Designates the list as a cirular list 
	GeneralList() {
		// Initialize list
		circular = false; // By default, lists are non-circular
		head = 0;
		tail = 0;
		cur = 0;
		length = 0;  // Maintains a list
	}
	void add_front(T data) {
		/*Adds a node to the front of the list*/
		length++;
		ListNode<T>* node = new ListNode<T>;
		node->data = data;
		node->next = head;
		node->prev = 0;
		if (head) head->prev 
			= node;
		head = node;
		if (!cur || !tail) {
			cur = node;
			tail = node;
		}
		if (circular) {
			node->prev = tail;
			tail->next = node;
		}
	}
	void add_back(T data) {
		/*Adds a node to the back of the list*/
		ListNode<T>* node = new ListNode<T>;
		node->data = data;
		node->prev = tail;
		node->next = 0;
		if (tail) tail->next = node;
		tail = node;
		if (!cur || !head) {
			cur = node;
			head = node;
		}
		if (circular) {
			node->next = head;
			head->prev = node;
		}
		length++;
	}
	void next() {
		/*Push current pointer to next item in the list*/
		if (cur->next) {
			cur = cur->next;
		}
		else {
			// cout << "Warning: End of List" << endl;
			cur = cur->next;
		}
	}
	
	void prev() {
		/*Push current pointer to previous item in the list*/
		if (cur->prev) {
			cur = cur->prev;
		}
		else {
			cout << "Warning: End of List" << endl;
			cur = cur->prev;
		}
	}

	void reset() {
		/*Set cur back to the head of the list*/
		cur = head;

	}

	void del() {
		/* Delete the node at Current*/
		ListNode<T>* temp = cur;
		if (cur == head && cur == tail) {
			// If the list consists of only 1 entry,
			// delete it and reinitialize.
			head = 0;
			tail = 0;
			cur = 0;
			delete temp;
		}
		else if (cur == head) {
			cur = cur->next;
			head = cur;
			cur->prev = 0;
			if (circular) cur->prev = tail;
			delete temp;
		}
		else if (cur == tail) {
			cur = cur->prev;
			tail = cur;
			cur->next = 0;
			if (circular) cur->next = head;
			delete temp;
		}
		else {
			cur = cur->next;
			temp->prev->next = temp->next;
			temp->next->prev = temp->prev;
			delete temp;
		}
		length--;
	}
	ListNode<T>* pop() {
		ListNode<T>* temp;
		if (!head) return NULL;
		temp = head;
		head = head->next;
		if (head) {
			head->prev = 0;
		}
		else {
			head = 0;
		}
		
		if (circular) head->prev = tail;
		length--;
		return temp;
	}
};

GeneralList<Token*>* QueueFile(string fileName, bool circular) {
	/*This function will file an input file and parse it into tokens by space,
	newline, and tab. The resulting sequence of tokens are stored within the
	string queue inputQueue, which is returned.
	Note: By storing the entire file in a queue, this method is inefficient from
	a memory perspective. However, given my as of yet limited capabilities, and
	an assumption that the input file is sufficiently small, this was determined
	to be acceptable for this lab. */
	char c;
	string token = "";
	int lineNum = 1;
	int offsetNum = 1;
	GeneralList<Token*>* tokenList = new GeneralList<Token*>; // final returned queue of tokens
	tokenList->circular = circular;
	ifstream myfile(fileName.c_str());

	if (myfile.is_open())
	{
		while (myfile.get(c))
		{
			if (c == '\t' || c == ' ' || c == '\n') {
				offsetNum++;
				if (!token.empty()) {
					Token* pTok = new Token;
					pTok->token = token;
					pTok->lineNum = lineNum;
					pTok->offsetNum = offsetNum - token.length() - 1;
					tokenList->add_back(pTok);
					token = "";
				}
			}
			else {
				offsetNum++;
				token += c;
			}
			if (c == '\n') {
				lineNum++;
				offsetNum = 1;
			}
		}
		myfile.close();
		/* In case there is no last delimiter before EOF*/
		if (!token.empty()) {
			Token* pTok = new Token;
			pTok->token = token;
			pTok->lineNum = lineNum;
			pTok->offsetNum = offsetNum - token.length() - 1;
			tokenList->add_back(pTok);
			token = "";
		}
	}
	else {
		cout << "Cannot Open Input File " << "fileName" << endl;
		exit(EXIT_FAILURE);
	}
	return tokenList;
}

vector<Process*> CreateProcessQueue(string filename) {
	vector<Process*> PCB;
	GeneralList<Token*>* inputList;
	string tokenValue;
	int pid = 0;
	inputList = QueueFile(filename, false);
	while (inputList->cur != inputList->tail) {
		Process* pProcess = new Process;
		pProcess->pid = pid; pid++;
		// Read in the processes, one token at a time
		pProcess->AT = readInt(inputList->cur->data);
		inputList->next();
		pProcess->TC = readInt(inputList->cur->data);
		pProcess->aTC = pProcess->TC; // set the static arrival time for delta calculations
		inputList->next();
		pProcess->CB = readInt(inputList->cur->data);
		inputList->next();
		pProcess->IO = readInt(inputList->cur->data);
		if (inputList->cur != inputList->tail) inputList->next();
		PCB.push_back(pProcess);
	}
	return PCB;
}

int myRandom(int burst, GeneralList<Token*>* randomList) {
	int out;
	out = 1 + readInt(randomList->cur->data) % burst;
	randomList->next();
	return out;
}

void bootStrapEvents(GeneralList<Event*>* eventList, vector<Process*> PCB, GeneralList<Token*>* randomList) {

	for (int i = 0; i < PCB.size(); i++) {
		Event * evt = new Event;
		evt->transition = Event::TRANS_TO_READY;
		evt->process = PCB[i];
		evt->timestamp = PCB[i]->AT;
		evt->process->staticPriority = myRandom(NUMPRIOS, randomList);
		evt->process->dynamicPriority = evt->process->staticPriority - 1;
		eventList->add_back(evt);
	}
}

Event* getEvent(GeneralList<Event*>* eventList) {
	Event* temp;
	if (eventList->head) {
		temp = eventList->head->data;
		eventList->del();
		return temp;
	}
	else {
		return NULL;
	}
}

class SchedulerBase {
	/*Base class for schedulers*/
public:
	GeneralList<Process*>* readyQ;
	bool PrePrio;
	SchedulerBase() {
		PrePrio = false;
		readyQ = new GeneralList<Process*>;
	}
};

class FCFS: public SchedulerBase {
	/* First-Come-First-Serve Scheduler*/
public:
	void add_to_queue(Process* p) {
		readyQ->add_back(p);
	}
	
	Process* get_next_process() {
		if (readyQ->head == 0) return NULL;
		return readyQ->pop()->data;
	}
};

class LCFS : public SchedulerBase {
	/* Last-Come-First-Serve Scheduler*/
public:
	void add_to_queue(Process* p) {
		readyQ->add_front(p);
	}
	Process* get_next_process() {
		if (readyQ->head == 0) return NULL;
		return readyQ->pop()->data;
	}
};

class RR : public FCFS {
public:
	/* Round-Robin Scheduler
	   Same as FCFS, but with a given quantum*/
};

class SRTF : public SchedulerBase {
	/* Shortest Remaining Time First Scheduler*/
public:
	void add_to_queue(Process* p) {
		readyQ->add_back(p);
	}
	Process* get_next_process() {
		if (readyQ->head == 0) return NULL;
		Process * ShortProc = NULL;  // Shortest Process in Q
		int ShortTime = 999999; // Shorest amount of remaining time
		readyQ->reset();  // Reset the cur pointer within ReadyQ
		if (verbose) cout << "SRTF Scheduler call:" << endl;
		do {
			// Search for process with smallest remaining time
			if (verbose) {
				cout << "pid=" << readyQ->cur->data->pid << " ";
				cout << "REM=" << readyQ->cur->data->TC << endl;
			}
			
			if (readyQ->cur->data->TC < ShortTime) {
				ShortTime = readyQ->cur->data->TC;
				ShortProc = readyQ->cur->data;
			}
			if (readyQ->cur == readyQ->tail) break;
			readyQ->next();
		} while (readyQ->cur);

		if (!ShortProc) {
			// Abort if it cannot find a process; for debugging
			cout << "Error in LCFS: No shortest process found" << endl;
			exit(-1);
		}
		if (verbose) {
			cout << "Returning:" << " ";
			cout << "pid=" << ShortProc->pid << " ";
			cout << "REM=" << ShortProc->TC << endl;
		}

		readyQ->reset();
		do {
			// Remove the chosen process form the queue
			if (readyQ->cur->data == ShortProc) {
				readyQ->del();
				break;
			}
			if (readyQ->cur == readyQ->tail) break;
			readyQ->next();
		} while (readyQ->cur);

		 // Remove the current (at the shortest) process
		return ShortProc;
	}
};

class PRIO : public SchedulerBase {
	/* Priority, no preemption Scheduler*/
public:
	GeneralList<Process*> sched_arrays[2][NUMPRIOS];
	bool active;
	bool expired;
	// GeneralList<Process*> *active = &sched_arrays[0][0];
	// GeneralList<Process*> *expiredQ = &sched_arrays[1][0];
	PRIO() {
		active = false;
		expired = true;

	}
	bool isPrioEmpty(int state) {
		// Checks if a state (active or expired) queue is empty
		for (int i = 0; i < NUMPRIOS; i++) {
			if (sched_arrays[state][i].head) return false;
		}
		return true;
	}
	void add_to_queue(Process* p) {

		/*
		if (p->dynamicPriority < 0) {
			p->dynamicPriority = p->staticPriority - 1;
			sched_arrays[expired][p->dynamicPriority].add_back(p);
		}
		*/
		if (p->PriorityReset) {
			// if it has previously run, decrement prio
			p->PriorityReset = false;
			p->dynamicPriority -= 1;
		}
		if (p->dynamicPriority < 0) {
			// If process reaches bottom of queue, reset prio
			p->dynamicPriority = p->staticPriority - 1;
			sched_arrays[expired][p->dynamicPriority].add_back(p);
		}
		else {
			sched_arrays[active][p->dynamicPriority].add_back(p);
		}
		
	}

	Process* get_next_process() {
		Process* temp = NULL;
		bool ActiveEmpty = isPrioEmpty(active);
		bool ExpiredEmpty = isPrioEmpty(expired);
		// if both active and expired  are empty, then entire readyQ is empty
		if (verbose) {
			cout << "Priority Scheduler: AE=" << ActiveEmpty << " EE=" << ExpiredEmpty << endl;
		}
		if (ActiveEmpty && ExpiredEmpty) return NULL;
		if (ActiveEmpty) {
			// if active is empty and expired not, then switch
			active = !active;
			expired = !expired;
			if (verbose) {
				cout << " Switching..." << endl;
			}
		}
		// Find next process
		for (int i = NUMPRIOS - 1; i >= 0; i--) {
			if (!sched_arrays[active][i].head) continue;
			temp = sched_arrays[active][i].pop()->data;
			break;
		}
		if (verbose) {
			cout << "Found pid=" << temp->pid << " PRIO=" << temp->dynamicPriority << endl;
		}
		temp->PriorityReset = true;
		
		return temp;
	}
};

class PREPRIO : public PRIO {
	/* Priority with preemption Scheduler
	   Same as PRIO, but with only the
	   PrePrio flag set to true in order
	   to invoke the simulator's premption
	   logic*/
public:
	PREPRIO() {
		PrePrio = true;
	}
};

void insertEvent(Event* evt, GeneralList<Event*>* eventList) {
	/* Inserts an event into the event queue based on timestamp.
	   if an event is already in the event queue at the same time, 
	   then the new event is inserted after*/
	eventList->reset();  // reset the cur pointer to head
	if (!eventList->head) {
		// Special case: list is empty, then just add to head
		eventList->add_front(evt);
		return;
	}
	if (eventList->head == eventList->tail) {
		// Special case: list is only one element
		if (eventList->cur->data->timestamp <= evt->timestamp) {
			eventList->add_back(evt);
		}
		else {
			eventList->add_front(evt);
		}
		return;
	}



	while (eventList->cur != eventList->tail && eventList->cur->data->timestamp <= evt->timestamp) {
		// General case
		// loop through list until either the tail or the  right spot is found
		eventList->next();
	}
	if (eventList->cur == eventList->tail) {

		if (eventList->cur->data->timestamp <= evt->timestamp) {
			eventList->add_back(evt);
		}
		else  {
			// insert in front of cur
			ListNode<Event*>* newNode = new ListNode<Event*>;
			newNode->data = evt;
			newNode->next = eventList->cur;
			newNode->prev = eventList->cur->prev;
			eventList->cur->prev->next = newNode;
			eventList->cur->prev = newNode;
		}

	}
	else {
		// If it's not the tail of the eventlist
		if (eventList->cur == eventList->head) {
			eventList->add_front(evt);
		}
		else {
			// If in middle of the list, insert in front of cur
			ListNode<Event*>* newNode = new ListNode<Event*>;
			newNode->data = evt;
			newNode->next = eventList->cur;
			newNode->prev = eventList->cur->prev;
			eventList->cur->prev->next = newNode;
			eventList->cur->prev = newNode;

		}
	}
	
	eventList->length++;
}

Process* insertPrePrioEvent(Process* proc, Process* CURRENT, GeneralList<Event*>* eventList, int quantum) {
	if (verbose) {
		cout << "Prempting process pid=" << CURRENT->pid << " PRIO=" << CURRENT->dynamicPriority;
		cout << " with pid=" << proc->pid << " PRIO=" << proc->dynamicPriority << endl;
	} 
	// Check to see if there is an block/prempt event for the current process already
	// scheduled at this timestep
	eventList->reset();
	do {
		if (eventList->cur->data->process == CURRENT && eventList->cur->data->timestamp == CURRENT_TIME &&
			(eventList->cur->data->transition == Event::TRANS_TO_BLOCK ||
				eventList->cur->data->transition == Event::TRANS_TO_PREEMPT || 
				eventList->cur->data->transition == Event::TRANS_TO_TERM)) {
			// Current process was going to block this round anyways, so disregard the premption
			if (verbose) cout << "Process pid=" << CURRENT->pid << " already ending; skipping removal" << endl;
			eventList->reset();
			return CURRENT;
		}
		eventList->next();
	} while (eventList->cur && eventList->cur->data->timestamp == CURRENT_TIME);

	// CURRENT->prevStateTC = CURRENT_TIME - CURRENT->stateTC;
	// CURRENT->stateTC = CURRENT_TIME;
	CURRENT->stateCB -= CURRENT_TIME - CURRENT->stateTC;
	// Step 1: Remove the next transition of the running process
	eventList->reset();
	do {
		if (eventList->cur->data->process == CURRENT) {
			if (eventList->cur->data->transition == Event::TRANS_TO_BLOCK) {
				// Set back the amount of CB time missed
				if (verbose) {
					cout << "Removing BLOCK transition at TS=" << eventList->cur->data->timestamp;
					cout << " and restoring " << eventList->cur->data->timestamp - CURRENT_TIME << " to TC" << endl;
				}
				CURRENT->TC += eventList->cur->data->timestamp - CURRENT_TIME;
				//CURRENT->stateCB += eventList->cur->data->timestamp - CURRENT_TIME;
				eventList->del();
				break;
			}
			if (eventList->cur->data->transition == Event::TRANS_TO_PREEMPT) {
				// Set back the amount of CB time missed
				if (verbose) {
					cout << "Removing PREEMPT transition at TS=" << eventList->cur->data->timestamp;
					cout << " and restoring " << eventList->cur->data->timestamp - CURRENT_TIME << " to TC" << endl;
				}
				CURRENT->TC += eventList->cur->data->timestamp - CURRENT_TIME;
				// CURRENT->stateCB += eventList->cur->data->timestamp - CURRENT_TIME;
				eventList->del();
				break;
			}
			if (eventList->cur->data->transition == Event::TRANS_TO_TERM) {
				// Set back the amount of CB time missed
				if (verbose) {
					cout << "Removing TERM transition at TS=" << eventList->cur->data->timestamp << endl;
					// cout << " and restoring " << eventList->cur->data->timestamp - CURRENT_TIME << " to TC" << endl;
				}
				CURRENT->TC -= eventList->cur->data->timestamp - CURRENT_TIME;
				CURRENT->stateCB -= eventList->cur->data->timestamp - CURRENT_TIME;
				eventList->del();
				break;
			}
		}	
		eventList->next();
	} while (eventList->cur);
	// Step 2: Create a new prempted event for the current process
	CURRENT->Prempted = true; // Note process was prempted on last run
	CURRENT->PriorityReset = true;
	CURRENT->PremptTime = 0;
	// CURRENT->PremptTime = CURRENT_TIME - CURRENT->stateTC;
	// Create a TRANS_TO_PREEMPT event
	Event * newEvent = new Event;
	newEvent->process = CURRENT;
	newEvent->timestamp = CURRENT_TIME;
	newEvent->transition = Event::TRANS_TO_PREEMPT;
	insertEvent(newEvent, eventList);
	// Step 3: create a new event to set the new process to run
	proc->PriorityReset = true;
	proc->Prempted = false;
	Event * newerEvent = new Event;
	newerEvent->process = proc;
	newerEvent->timestamp = CURRENT_TIME;
	newerEvent->transition = Event::TRANS_TO_RUN;
	insertEvent(newerEvent, eventList);

	return proc;
	
}

template<typename S>
void Simulation(int quantum, GeneralList<Event*>* eventList, vector<Process*> PCB, S THE_SCHEDULER, GeneralList<Token*>* randomList) {
	Event* evt;
	int ranBurst;
	bool CALL_SCHEDULER = false;
	Process* CURRENT_RUNNING_PROCESS = NULL;
	Process* PrePriorProc = NULL; // Process to prempt the current running process
	Process * proc = NULL;
	while (eventList->head) {
		 
		if (verbose) {
			cout << "> Event List:" << endl;
			eventList->reset();
			do {
				cout << "> Event TS=" << eventList->cur->data->timestamp;
				cout << " pid=" << eventList->cur->data->process->pid;
				cout << " T=" << eventList->cur->data->transitionToString(eventList->cur->data->transition) << endl;
				eventList->next();
			} while (eventList->cur);
			eventList->reset();
		}
		
		evt = eventList->pop()->data;
		eventList->reset();
		Event * newEvent = new Event;
		proc = evt->process;
		if (evt->timestamp < CURRENT_TIME) {
			// Enforce chonology
			cout << "SPACETIME BROKEN!!! Summon the ghost of Einstein!" << endl;
			exit(-1);
		}
		CURRENT_TIME = evt->timestamp;
		proc->prevStateTC = CURRENT_TIME - proc->stateTC;
		proc->stateTC = CURRENT_TIME;
		switch (evt->transition) {
		case Event::TRANS_TO_READY:
			
			// Reset the PRIO for when it returns from IO
			if (proc->Status == Process::BLOCKED) {
				proc->dynamicPriority = proc->staticPriority - 1;
				proc->PriorityReset = false;
			}
			proc->setStatus(Process::READY);
			if (THE_SCHEDULER->PrePrio) {
				/* Additional Logic for PREPRIO Scheduler */
				if (CURRENT_RUNNING_PROCESS && proc->dynamicPriority > CURRENT_RUNNING_PROCESS->dynamicPriority) {
					
					CURRENT_RUNNING_PROCESS = insertPrePrioEvent(proc, CURRENT_RUNNING_PROCESS, eventList, quantum);
					CALL_SCHEDULER = false;
					if (CURRENT_RUNNING_PROCESS != proc) {
						// if the premption was skipped due to the event already ending in this
						// timestep, then process the premptining process like a regular process
						THE_SCHEDULER->add_to_queue(proc);
						CALL_SCHEDULER = true;

					}
				}
				else {
					// If it's a lower priority or there isn't a process running, then
					// add to the scheduler normally.
					THE_SCHEDULER->add_to_queue(proc);
					CALL_SCHEDULER = true;
				}
			}
			else {
				// If this is not PREPRIO, then add the ready process to the scheduler
				THE_SCHEDULER->add_to_queue(proc);
				CALL_SCHEDULER = true;
			}
			
			break;
		case Event::TRANS_TO_RUN:
			if (!proc->Prempted) {
				// If the process was blocked last time, get a new CB
				proc->stateCB = myRandom(proc->CB, randomList);  // Calculate CPU Burst
			}
			else {
				// If process was prempted, continue the remainder of the last CB
				proc->stateCB -= proc->PremptTime;
				proc->Prempted = false;

			}
			proc->setStatus(Process::RUNNING);
			if (proc->TC - proc->stateCB <= 0 && proc->TC <= quantum) {
				// Create a TRANS_TO_TERM event
				newEvent->process = proc;
				newEvent->timestamp = CURRENT_TIME + proc->TC;
				newEvent->transition = Event::TRANS_TO_TERM;
				insertEvent(newEvent, eventList);
				
			}
			else if (proc->stateCB <= quantum) {
				// A preemption hasn't occured, therefore block
				proc->Prempted = false; // Note the process wasn't prempted on last run
				proc->TC -= proc->stateCB; // Decrease remaining runtime
				
				// Create a TRANS_TO_BLOCK event
				newEvent->process = proc;
				newEvent->timestamp = CURRENT_TIME + proc->stateCB;
				newEvent->transition = Event::TRANS_TO_BLOCK;
				insertEvent(newEvent, eventList);
			}
			else {
				// If a preemption occurs
				proc->Prempted = true; // Note process was prempted on last run
				proc->PremptTime = quantum; // set the time it was prempted for
				proc->TC -= quantum; // Decrease remaining runtime
				// Create a TRANS_TO_PREEMPT event
				newEvent->process = proc;
				newEvent->timestamp = CURRENT_TIME + quantum;
				newEvent->transition = Event::TRANS_TO_PREEMPT;
				insertEvent(newEvent, eventList);
			}
			break;
		case Event::TRANS_TO_BLOCK:
			CALL_SCHEDULER = true;
			
			CURRENT_RUNNING_PROCESS = NULL;
			proc->stateIB = myRandom(proc->IO, randomList);  // Calculate IO Burst
			proc->setStatus(Process::BLOCKED);
			// Create a TRANS_TO_READY event
			newEvent->process = proc;
			newEvent->timestamp = CURRENT_TIME + proc->stateIB;
			newEvent->transition = Event::TRANS_TO_READY;
			insertEvent(newEvent, eventList);
			break;
		case Event::TRANS_TO_PREEMPT:
			CALL_SCHEDULER = true;
			proc->setStatus(Process::PREMPT);
			if (CURRENT_RUNNING_PROCESS == proc) {
				// prevents stoping a different process
				// if preempted by PremptPrio
				CURRENT_RUNNING_PROCESS = NULL;
			}
			// Create a TRANS_TO_READY event
			newEvent->process = proc;
			newEvent->timestamp = CURRENT_TIME;
			newEvent->transition = Event::TRANS_TO_READY;
			insertEvent(newEvent, eventList);
			break;
		case Event::TRANS_TO_TERM:
			CALL_SCHEDULER = true;
			// if the process terminates, remove and set to terminate
			proc->setStatus(Process::TERMINATED);
			CURRENT_RUNNING_PROCESS = NULL;
			break;
		} 
		delete evt;
		evt = NULL;

		if (CALL_SCHEDULER) {
			if (eventList->head && eventList->head->data->timestamp == CURRENT_TIME) {
				continue;
			}
			CALL_SCHEDULER = false;
			if (CURRENT_RUNNING_PROCESS == NULL) {
				CURRENT_RUNNING_PROCESS = THE_SCHEDULER->get_next_process();
				if (CURRENT_RUNNING_PROCESS == NULL) continue;
				// Create a TRANS_TO_RUN event
				Event * newerEvent = new Event;
				newerEvent->process = CURRENT_RUNNING_PROCESS;
				newerEvent->timestamp = CURRENT_TIME;
				newerEvent->transition = Event::TRANS_TO_RUN;
				insertEvent(newerEvent, eventList);
				
			}
		}
	}
	if (proc) {
		// Add the termination time of the last
		// process to the IO downtime
		IODownTime += CURRENT_TIME - IODownTS;
	}
}

string padAddress(int num) {
	// Converts an int to a properly padded string
	ostringstream ss;
	ss << setw(4) << setfill('0') << num;
	return ss.str();
}
int readIntPlain(string strNumber) {
	int outInt;
	stringstream streamConverter(strNumber);
	streamConverter >> outInt;

	return outInt;
}

main(int argc, char **argv)
{
	// Initialize placeholders
	enum Schedulers { iFCFS, iLCFS, iRR, iSRTF, iPRIO, iPREPRIO};
	string schedName[] = { "FCFS", "LCFS", "RR", "SRTF", "PRIO", "PREPRIO"};
	string randName = "/home/cjg507/projects/Lab2/lab2_assign/rfile";
	string inpName = "/home/cjg507/projects/Lab2/lab2_assign/input3";
	int schedChoice = iFCFS;
	int quantum = 10000;
	
	// Get options
	stringstream ss;
	string s; // Placeholder string for input variables
	
	int index;
	int c;
	opterr = 0;
	
	
	while ((c = getopt (argc, argv, "vs:")) != -1) {
    switch (c)
      {
      case 'v':
        verbose = true;
        break;
      case 's':
		ss << optarg;
		ss >> s;
        if (s.substr(0,1) == "F") {
				schedChoice = iFCFS;
			}
			else if (s.substr(0,1) == "L") {
				schedChoice = iLCFS;
			}
			else if (s.substr(0,1) == "S") {
				schedChoice = iSRTF;
			}
			else if (s.substr(0,1) == "R") {
				schedChoice = iRR;
				quantum = readIntPlain(s.substr(1));
			}
			else if (s.substr(0,1) == "P") {
				schedChoice = iPRIO;
				quantum = readIntPlain(s.substr(1));
			}
			else if (s.substr(0,1) == "E") {
				schedChoice = iPREPRIO;
				quantum = readIntPlain(s.substr(1));
			}
			else {
				cout << "Error: " << s.substr(0,1) << " not a valid scheduler" << endl;
				exit(1);
			}
        break;
      case '?':
        if (optopt == 's')
          fprintf (stderr, "Option -%c requires an argument.\n", optopt);
        else if (isprint (optopt))
          fprintf (stderr, "Unknown option `-%c'.\n", optopt);
        else
          fprintf (stderr,
                   "Unknown option character `\\x%x'.\n",
                   optopt);
        return 1;
      default:
        abort ();
      }
	}
	// read in input file
	inpName = argv[optind];
	// read in random file
	randName = argv[optind + 1];
	

	// Read Random Number List
	GeneralList<Token*>* randomList;
	randomList = QueueFile(randName, true);
	randomList->del(); // Remove the first index entry

	// Read input processes
	vector<Process*> PCB; 
	PCB = CreateProcessQueue(inpName);

	// Bootstrap Event list with input processes
	GeneralList<Event*>* eventList = new GeneralList<Event*>;
	bootStrapEvents(eventList, PCB, randomList);

	// Set Scheduler and run simulation
	switch (schedChoice)
	{
	case iFCFS:
		{FCFS* THE_SCHEDULER = new FCFS;
		quantum = 10000;
		Simulation<FCFS*>(quantum, eventList, PCB, THE_SCHEDULER, randomList); }
		break;
	case iLCFS:
		{LCFS* THE_SCHEDULER = new LCFS;
		quantum = 10000;
		Simulation<LCFS*>(quantum, eventList, PCB, THE_SCHEDULER, randomList); }
		break;
	case iRR:
		{RR* THE_SCHEDULER = new RR;
		Simulation<RR*>(quantum, eventList, PCB, THE_SCHEDULER, randomList); }
		break;
	case iSRTF:
		{SRTF* THE_SCHEDULER = new SRTF;
		quantum = 10000;
		Simulation<SRTF*>(quantum, eventList, PCB, THE_SCHEDULER, randomList); }
		break;
	case iPRIO:
		{PRIO* THE_SCHEDULER = new PRIO;
		Simulation<PRIO*>(quantum, eventList, PCB, THE_SCHEDULER, randomList); }
		break;
	case iPREPRIO:
		{PREPRIO* THE_SCHEDULER = new PREPRIO;
		Simulation<PREPRIO*>(quantum, eventList, PCB, THE_SCHEDULER, randomList); }
		break;
	default:
		break;
	}
	
	// Output the scheduler choice
	cout << endl << schedName[schedChoice] << " ";
	if (quantum != 10000) {
		cout <<  quantum << " ";
	}
	cout << endl;

	// Output Final Stats
	if (verbose) {
		// Extra table key for easy readying in verbose
		cout << "pid" << ":\t";
		cout << "AT" << "\t";
		cout << "TC" << "\t";
		cout << "CB" << "\t";
		cout << "IO" << "\t";
		cout << "PRIO" << "\t|\t";
		cout << "FT" << "\t";
		cout << "TT" << "\t";
		cout << "IT" << "\t";
		cout << "CW" << endl;

	}

	int SumTT = 0;
	int SumCW = 0;
	// Process' Outputs
	for (int i = 0; i < PCB.size(); i++) {
		cout << padAddress(PCB[i]->pid) << ":\t";
		cout << PCB[i]->AT << "\t";
		cout << PCB[i]->aTC << "\t";
		cout << PCB[i]->CB << "\t";
		cout << PCB[i]->IO << "\t";
		cout << PCB[i]->staticPriority << "\t|\t";
		cout << PCB[i]->FT << "\t";
		cout << PCB[i]->TT << "\t"; SumTT += PCB[i]->TT;
		cout << PCB[i]->IT << "\t";
		cout << PCB[i]->CW << endl; SumCW += PCB[i]->CW;
	}
	// Final Summary info
	if (verbose) {
		// Extra table key for easy readying in verbose
		cout << "SUM" << ":\t";
		cout << "LastFT" << "\t";
		cout << "CPUU\%" << "\t";
		cout << "IOU\%" << "\t";
		cout << "AveTT" << "\t";
		cout << "AveCW" << "\t";
		cout << "TPper100" << endl;
	}
	double CPUUtil = (100.0 - RunDownTime * 100.0 / (TotalTime * 1.0));
	double IOUtil = 100 - 100.0*(IODownTime)/(TotalTime*1.0);
	double AveTT = (SumTT*1.0 / (PCB.size() * 1.0));
	double AveCW = (SumCW*1.0 / (PCB.size() * 1.0));
	double TPper100 = (PCB.size()*100.0 / (TotalTime * 1.0));
	
	cout << "SUM" << ":\t";
	cout << TotalTime << "\t";
	printf("%.2f \t", CPUUtil);
	printf("%.2f \t", IOUtil);
	printf("%.2f \t", AveTT);
	printf("%.2f \t", AveCW);
	printf("%.3f \n", TPper100);

	

	return 0;
}

