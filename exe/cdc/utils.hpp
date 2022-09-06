#pragma once
#include <sys/stat.h>


namespace json = boost::json; // this needs to be here to allow the definition of the json object


using namespace std;

// Utility method to see if an element is in a vector
template <class Vector, class Element> bool contains(Vector v, Element x) {
    return std::find(v.begin(), v.end(), x) != v.end();

}
string label_map[][2] = {{"CriticalVictim","Move"},{"HelpRequest","HelpOffer"},{"HelpRequest","Move"},{"CriticalVictim","RescueInteractions"},{"Block","Engineer"}};
 int map_rows =  sizeof label_map / sizeof label_map[0];
//label_map["CriticalVictim"]="MoveTo";


// Utility function for starting the log-file
 // this is called in the agent constructor
 // creates a logs directory in ../logs/
 void createLog(){
    if (mkdir("../logs", 0777) == -1)
        cerr << "Error :  " << strerror(errno) << endl;
  
    else
        cout << "Directory created";

    ofstream file("../logs/cdc-log.txt");
    file << "------------Begin Log------------" << "\n\n";
    file.close();
 }

/** This function takes 3 arguments:
 * a queue state 
 * an index 
 * a json object
 * --- operation: 
 * writes all utterances in  the queue state up until the index point into a log file
 * also writes the relevant labels*/
void writeToLog(deque<json::object> utterance_queue,int index, json::object evidence) {
    fstream file;
    file.open("../logs/cdc-log.txt",ios::app); //opening the file in append mode
    file << "|CDC sequence detected|\n";
    file << evidence <<"\n";
    for (size_t i = 0; i < index+1; i++) {
    file << "Utterance " << i << ":\n";
    file << utterance_queue.at(i).at("data")
                                .at("text").as_string()
        << "\n";

    }
    file << "------- end of utterance sequence for " << evidence << " --------" <<"\n\n";

    file.close();

}
