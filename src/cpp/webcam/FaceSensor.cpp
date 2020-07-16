#include <iostream>
#include <csignal>
#include "WebcamSensor.h"
#include <boost/program_options.hpp>

using namespace std;
using namespace tomcat;
namespace po = boost::program_options;


int main(int ac, char* av[]) 
{
	string exp_id, trial_id, playername;
	// Boost command line options
	try {
	
		po::options_description desc("Allowed options");
		desc.add_options()
			("help", "produce help message")
			("exp_id", po::value<string>(&exp_id)->default_value("null"), "set experiment ID")
			("trial_id", po::value<string>(&trial_id)->default_value("null"), "set trial ID")
			("playername", po::value<string>(&playername)->default_value("null"), "set playername")
		;
		
		po::variables_map vm;
		po::store(po::parse_command_line(ac,av,desc), vm);
		po::notify(vm);
		
		if (vm.count("help")) {
    		cout << desc << "\n";
    		return 0;
		}
		
	}
	catch(exception& e) {
        cerr << "error: " << e.what() << endl;
        return 1;
    }
    catch(...) {
        cerr << "Exception of unknown type!" << endl;
    }
		
	
    WebcamSensor camsensor;
    camsensor.initialize(exp_id, trial_id, playername);
    camsensor.get_observation();
    
    return 0;
  
}
