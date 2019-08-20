/* Operating Systems
Cody Gilbert
10/2/2018 */


#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <queue>
#include <sstream>
#include <stdlib.h>
#include <vector>
#include <iomanip>
using namespace std;

class Token {
public:
	string token;
	int lineNum;
	int offsetNum;
};



class Symbol {
	/* Class for storing variable symbols*/
public:
	string name;
	string E;
	int val;
	int modNum;
	int modRefSize;
	int useFlag;
	Symbol() {
		E = "";
		useFlag = -1;
	}
};

class Address {
	/* Class for storing code addresses*/
public:
	string oldAdd;
	string newAdd;
	string E;
	Address() {
		E = "";
	}
	void standardAddress() {
		while (oldAdd.substr(0, 1) == "0") {
			oldAdd = oldAdd.substr(1);
		}
		while (oldAdd.length() < 4) {
			oldAdd = "0" + oldAdd;
		}
	}
};

void __parseerror(int errcode, Token* errTok) {
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
		__parseerror(0, token);
	}
	stringstream streamConverter(strNumber);
	streamConverter >> outInt;

	return outInt;
}

int readIntPlain(string strNumber) {
	// Like readInt, but only accepts a string. Assumes
	// string already valid.
	int outInt; // output int if acceptable
	stringstream streamConverter(strNumber);
	streamConverter >> outInt;

	return outInt;
}

void checkSymbol(Token* token) {
	string sym = token->token;
	if (sym.find_first_not_of("0123456789") == string::npos) {
		// Raise a SYM_EXPECTED error
		__parseerror(1, token);
	}
	if (sym.length() > 16) {
		// Raise a SYM_TOO_LONG error
		__parseerror(3, token);
	}
	return;
}

void checkInstruction(Token* token) {
	/* Validates the instruction */
	string sym = token->token;
	if (sym == "I" | sym == "A" | sym == "R" | sym == "E") {
		return;
	}
	else {
		// Raise an ADDR_EXPECTED error
		__parseerror(2, token);
	}
}

void checkWord(Token* token, int &fileWords, string &inst) {
	/* Validates the properties of a given word*/
	int instNumber;
	int totNumber;
	string sym = token->token;
	if (sym.find_first_not_of("0123456789") != string::npos) {
		// If not a number, raise ADDR_EXPECTED
		__parseerror(2, token);
	}

	instNumber = readIntPlain(sym.substr(1));

	// Validate the object within 512 words of memory, based on instruction:
	if (inst == "A" | inst == "E") {
		totNumber = fileWords + instNumber;
	} 
	else if (inst == "R") {
		totNumber = instNumber;
	}
	else { // If an I, Immediate address
		totNumber = 0;  // Ignore immediate addresses
	}
}

string intToAddress(int num) {
	// Converts an int to a properly padded string
	ostringstream ss;
	ss << setw(3) << setfill('0') << num;
	return ss.str();
}

queue<Token*> QueueFileMan(string fileName) {
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
	queue<Token*> inputQueue; // final returned queue of tokens
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
					inputQueue.push(pTok);
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
	}
	else {
		cout << "Cannot Open Input File \n";
		exit(EXIT_FAILURE);
	}

	return inputQueue;
}



class TokenQueue {
public:
	queue<Token*> inputQueue;
	Token last;

	TokenQueue(string& fileName) {
		last.lineNum = 1;
		last.offsetNum = 1;
		last.token = "";
		inputQueue = QueueFileMan(fileName);
	}
	void pop() {
		last = *inputQueue.front();
		last.offsetNum += last.token.length() + 1;
		inputQueue.pop();
	}
	Token* front() {
		return inputQueue.front();
	}
	int size() {
		return inputQueue.size();
	}
	bool empty() {
		return inputQueue.empty();
	}
};

void checkQueue(int flag, TokenQueue inputQueue) {

	if (inputQueue.size() < 1) {
		__parseerror(flag, &inputQueue.last);
	}
}

int main(int argc, char** argv) {
	if (argv[1] == 0) {
		cout << "No filename given!" << endl;
		exit(EXIT_FAILURE);
	}
	string fileName = argv[1];
	// queue<Token*> inputQueue;
	TokenQueue inputQueue(fileName);
	int fileWords = 0;  // Number of words in program
	int modNum = 1;  // Number of modules in program
	int tempInt; // Temp integer used for various purposes
	// string fileName = "C:\\Users\\Cody Gilbert\\source\\repos\\Lab\\test.txt";
	string tempString;
	
	vector<Address*> addressList;
	vector<string> warningList;
	string instruction;
	bool tempBool = false;

	// Parse 1
	// inputQueue = QueueFileMan(fileName);
	vector<Symbol*> symbolList;
	while (!inputQueue.empty()) {

		// Read definition list
		checkQueue(0, inputQueue);
		
		int Count = readInt(inputQueue.front());
		if (Count > 16) {
			// Raise a TOO_MANY_DEF_IN_MODULE error
			__parseerror(4, inputQueue.front());
		}
		inputQueue.pop();
		for (int i = 0; i < Count; i++) {  // Read each symbol-value pair
			Symbol *pSymbol = new Symbol();
			checkQueue(1, inputQueue);
			checkSymbol(inputQueue.front()); // Verify it's a symbol
			pSymbol->name = inputQueue.front()->token;
			inputQueue.pop();
			checkQueue(0, inputQueue);
			pSymbol->val = readInt(inputQueue.front());
			inputQueue.pop();
			pSymbol->modNum = modNum;
			tempBool = true; // Flag for non-duplicated symbols
			for (int i = 0; i < symbolList.size(); i++) {
				if (pSymbol->name == symbolList[i]->name) {
					tempString = "Error: This variable is multiple times defined; first value used";
					symbolList[i]->E = tempString;
					tempBool = false; // Flag for non-duplicated symbols
				}
			}
			if (tempBool) {  // Flag for non-duplicated symbols
				// If the symbol was already defined, do not insert it again
				symbolList.push_back(pSymbol);
				tempBool = true;
			}

		}
		
		// Update relative word sizes
		for (int i = 0; i < symbolList.size(); i++) {
			if (symbolList[i]->modNum == modNum) {
				symbolList[i]->modRefSize = fileWords;
			}
		}
		modNum++; //Track the number of modules

		// Read use list
		checkQueue(0, inputQueue);
		Count = readInt(inputQueue.front());
		if (Count > 16) {
			// Raise a TOO_MANY_USE_IN_MODULE error
			__parseerror(5, inputQueue.front());
		}
		inputQueue.pop();
		for (int i = 0; i < Count; i++) {  // Read each symbol-value pair
			checkQueue(0, inputQueue);
			checkSymbol(inputQueue.front()); // Verify each a symbol
			inputQueue.pop();
		}

		// Read program text list
		checkQueue(0, inputQueue);
		Count = readInt(inputQueue.front());
		fileWords += Count; // Increment file word size
		if (fileWords > 512) {
			// Raise a  TOO_MANY_INSTR error
			__parseerror(6, inputQueue.front());
		}
		for (int i = 0; i < symbolList.size(); i++) {
			// Verify each defined symbol in the module is within the module
			if (symbolList[i]->modNum == modNum - 1) {
				if (symbolList[i]->val > Count) {
					// if the symbol is defined outside of the module, set to 0 and flag
					tempString = "Warning: Module " + to_string(static_cast<long long>(modNum - 1)) + ": " + symbolList[i]->name;
					tempString += " too big " + to_string(static_cast<long long>(symbolList[i]->val)) + " (max=";
					tempString += to_string(static_cast<long long>(Count) - 1) + ") assume zero relative";
					warningList.push_back(tempString);
					symbolList[i]->val = 0;
				}
			}
		}
		inputQueue.pop();
		for (int i = 0; i < Count; i++) {  // Read each symbol-value pair
			checkQueue(2, inputQueue);
			instruction = inputQueue.front()->token;
			checkInstruction(inputQueue.front()); // Verify each instruction
			inputQueue.pop();
			checkQueue(0, inputQueue);
			checkWord(inputQueue.front(), fileWords, instruction);
			inputQueue.pop();
		}

	}
	fileWords = 0; // Reset Parameters
	// Parse 2
	modNum = 1;
	inputQueue = TokenQueue(fileName);
	while (!inputQueue.empty()) {
		// Read definition list
		int Count = readInt(inputQueue.front());
		inputQueue.pop();
		for (int i = 0; i < Count; i++) {  // Symbols already stored, skip
			inputQueue.pop();
			inputQueue.pop();
		}

		vector<Symbol*> moduleVars;
		// Read use list
		Count = readInt(inputQueue.front());
		inputQueue.pop();
		for (int i = 0; i < Count; i++) {  // Read each symbol-value pair
			for (int j = 0; j < symbolList.size(); j++) {
				// Check to see if the defined variables are used
				if (inputQueue.front()->token == symbolList[j]->name) {
					symbolList[j]->useFlag = modNum;
				}
			}
			Symbol* pModVar = new Symbol;
			pModVar->name = inputQueue.front()->token;
			moduleVars.push_back(pModVar);
			inputQueue.pop();
		}
		modNum++;

		
		// Read program text list
		Count = readInt(inputQueue.front());
		inputQueue.pop();
		for (int i = 0; i < Count; i++) {  // Read each symbol-value pair
			instruction = inputQueue.front()->token;
			inputQueue.pop();
			Address *pAddress = new Address();
			pAddress->oldAdd = inputQueue.front()->token;
			pAddress->standardAddress();
			inputQueue.pop();
			if (instruction == "E" && readIntPlain(pAddress->oldAdd.substr(1)) > moduleVars.size() - 1) {
				// if the external address is larger than the use list, treat as I
				pAddress->E = "Error: External address exceeds length of uselist; treated as immediate";
				instruction = "I";
			}
			if (readIntPlain(pAddress->oldAdd) > 9999 && instruction != "I") {
				// raise opcode error 
				pAddress->newAdd = intToAddress(9999);
				pAddress->E = "Error: Illegal opcode; treated as 9999";
				instruction = "NA";
			}
			if (instruction == "E") { // Word is E, find variable and change mem loc
				tempBool = true; // Defined-symbol Flag
				tempString = moduleVars[readIntPlain(pAddress->oldAdd.substr(1))]->name;
				moduleVars[readIntPlain(pAddress->oldAdd.substr(1))]->useFlag = 0; // mark var as used
				for (int j = 0; j < symbolList.size(); j++) {
					if (tempString == symbolList[j]->name) {
						tempInt = symbolList[j]->modRefSize + symbolList[j]->val;
						pAddress->newAdd = pAddress->oldAdd.substr(0, 1) + intToAddress(tempInt);
						tempBool = false; // Defined-symbol Flag
					}
				}
				if (tempBool) {
					// if the symbol wasn't defined (not in symbol list), use 0
					pAddress->newAdd = pAddress->oldAdd.substr(0, 1) + intToAddress(0);
					pAddress->E = "Error: " + tempString + " is not defined; zero used";
				}
			}
			else if (instruction == "R") { // Word is R, create relative address
				if (readIntPlain(pAddress->oldAdd.substr(1)) > Count) {
					// raise error if abs address larger than machine
					tempInt = fileWords;
					pAddress->newAdd = pAddress->oldAdd.substr(0, 1) + intToAddress(tempInt);
					pAddress->E = "Error: Relative address exceeds module size; zero used";
				}
				else {
					tempInt = readIntPlain(pAddress->oldAdd.substr(1)) + fileWords;
					pAddress->newAdd = pAddress->oldAdd.substr(0, 1) + intToAddress(tempInt);
				}

				
			}
			else if (instruction == "A") { // Word is A, check and do not alter
				if (readIntPlain(pAddress->oldAdd.substr(1)) > 512) {
					// raise error if abs address larger than machine
					pAddress->newAdd = pAddress->oldAdd.substr(0, 1) + intToAddress(0);
					pAddress->E = "Error: Absolute address exceeds machine size; zero used";
				}
				else {
					pAddress->newAdd = pAddress->oldAdd;
				}
			}
			else if (instruction == "I") { // Instruction is I, check and do not alter it
				if (readIntPlain(pAddress->oldAdd) > 9999) {
					// raise error if abs address larger than allowed
					pAddress->newAdd = intToAddress(9999);
					pAddress->E = "Error: Illegal immediate value; treated as 9999";
				}
				else {
					pAddress->newAdd = pAddress->oldAdd;
				}
			}
			else {
				// Nothing to do if NA
			}
			
			addressList.push_back(pAddress);
			
		}
		fileWords += Count; // Increment file word size
		for (int i = 0; i < moduleVars.size(); i++) {
			// Verify all use list symbols were used
			if (moduleVars[i]->useFlag == -1) {
				tempString = "Warning: Module " + to_string(static_cast<long long>(modNum - 1)) + ": ";
				tempString += moduleVars[i]->name + " appeared in the uselist but was not actually used";
				warningList.push_back(tempString);
			}
		}
	}
	

	for (int j = 0; j < symbolList.size(); j++) {
		// Check to see if the defined variables are used
		if (symbolList[j]->useFlag == -1) {
			tempString = "Warning: Module " + to_string(static_cast<long long>(symbolList[j]->modNum)) + ": " + symbolList[j]->name;
			tempString += " was defined but never used";
			warningList.push_back(tempString);
		}
	}
	/*
	// Write Output File
	ofstream myfile("Output.txt");
	myfile << "Symbol Table" << endl;
	for (int i = 0; i < symbolList.size(); i++) {
		myfile << symbolList[i]->name << "=" << symbolList[i]->val + symbolList[i]->modRefSize;
		myfile <<  " " << symbolList[i]->E << endl;
	}
	myfile << endl << "Memory Map" << endl;
	for (int i = 0; i < addressList.size(); i++) {
		myfile << intToAddress(i) << ":  " << addressList[i]->newAdd;
		myfile << " " << addressList[i]->E << endl;
	}
	myfile << endl;
	for (int j = 0; j < warningList.size(); j++) {
		myfile << warningList[j] << endl;
	}
	myfile.close();
	*/
	// Print out symbol vector (testing)
	cout << "Symbol Table" << endl;
	for (int i = 0; i < symbolList.size(); i++) {
		cout << symbolList[i]->name << "=" << symbolList[i]->val + symbolList[i]->modRefSize;
		cout << " " << symbolList[i]->E << endl;
	}
	cout << endl << "Memory Map" << endl;
	for (int i = 0; i < addressList.size(); i++) {
		cout << intToAddress(i) << ":  " << addressList[i]->newAdd;
		cout << " " << addressList[i]->E << endl;
	}
	cout << endl;
	for (int j = 0; j < warningList.size(); j++) {
		cout << warningList[j] << endl;
	}

	return 0;
}