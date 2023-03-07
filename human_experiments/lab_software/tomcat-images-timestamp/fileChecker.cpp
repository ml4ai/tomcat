#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>
#include <sys/inotify.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>
#include <ctime>
#include <string.h>
#include <boost/program_options.hpp>

#include "publish.h"

#define MAX_EVENT_MONITOR 2048
#define NAME_LEN 32
#define MONITOR_EVENT_SIZE (sizeof(struct inotify_event))
#define BUFFER_LEN MAX_EVENT_MONITOR*(MONITOR_EVENT_SIZE+NAME_LEN)

namespace po = boost::program_options;

void outputFunc(int, char*, FILE**);

int main(int ac, char* av[]){
	int fd, watch_desc, out;
	char buffer[BUFFER_LEN];
	fd = inotify_init();
	std::string path;

	// Boost command line options
	try {
		po::options_description desc("Allowed options");
		desc.add_options()("help,h", "Show this help message")(
			"path",
			po::value<std::string>(&path)->default_value("/data/cat/LangLab/dev/file_checker"),
			"Set the path to be monitored")(
			"out",
			po::value<int>(&out)->default_value(0),
			"0 for stdout, 1 for file, 2 for mqtt");

		po::variables_map vm;
		po::store(po::parse_command_line(ac, av, desc), vm);
		po::notify(vm);

		if (vm.count("help")) {
			std::cout << desc << std::endl;
			return 0;
		}
	} catch (std::exception const& e) {
		std::cerr << "error: " << e.what() << std::endl;
		return 1;
	} catch (...) {
		std::cerr << "Exception of unknown type!" << std::endl;
		return 1;
	}
	
	char filePath[128];
	char outputMsg[300];
	strcpy(filePath, path.c_str());
	struct stat t_stat;
	struct timeval tv;
	FILE *outFile;
	std::string outPath = path + "/" + "outFile.csv";
	outFile = fopen(outPath.c_str(), "w"); // Change to a to append instead

	if (fd < 0)
		std::cerr << "Notify did not initialize";

	watch_desc = inotify_add_watch(fd, filePath, IN_CREATE);

	if (watch_desc == -1)
		std::cout << "Couldn't add watch to the path" << std::endl;
	else
		std::cout << "Monitoring path " << path << std::endl;

	while(true){
		int total_read = read(fd, buffer, BUFFER_LEN);
		if(total_read < 0)
			std::cerr << "Read error";

		int i = 0;	
		while(i < total_read){
			struct inotify_event *event = (struct inotify_event*) &buffer[i];
			if(event->len){
				if(event->mask & IN_CREATE){

					std::string filename = event->name;
					
					if (!(filename.substr(filename.find_last_of(".") + 1) == "png"))
						break;

					// Get the timeval data
					// gettimeofday(&tv, NULL);

					// Get the date-time of file creation
					char fileName[256];

					// Format the string to get the complete path of the file
					snprintf(fileName, sizeof(fileName), "%s/%s", filePath, event->name);
					// To print out the return value of stat: printf("%d\n", stat(fileName, &t_stat));
					stat(fileName, &t_stat);
					struct tm *timeinfo = gmtime(&t_stat.st_ctime);

					if(event->mask & IN_ISDIR)
						printf("Directory \"%s\" was created\n", event->name);
					else {
						//printf("%s: %ld.%09ld\n", event->name, t_stat.st_ctim.tv_sec, t_stat.st_ctim.tv_nsec);
						//printf("%s: %ld.%06ld\n", event->name, tv.tv_sec, tv.tv_usec);

						char ISOBuff[100];
						strftime(ISOBuff, sizeof(outputMsg), "%FT%T", timeinfo);

						snprintf(outputMsg, sizeof(outputMsg), "%s;%s.%09ldZ\n", event->name, ISOBuff, t_stat.st_ctim.tv_nsec);
						outputFunc(out, outputMsg, &outFile);
					}
				}
				i += MONITOR_EVENT_SIZE + event->len;
			}
		}
	}
	
	fclose(outFile);
	inotify_rm_watch(fd, watch_desc);
	close(fd);

	return 0;
}

// Function to output the timestamp in the format specified by the
// boost argument '--out'
void outputFunc(int choice, char* output, FILE** outFile) {
	switch(choice) {
		case 0:
			std::cout << output;
			break;

		case 1:
			if (*outFile) {
				fprintf(*outFile, "%s", output);
				fflush(*outFile);
			}
			break;

		case 2: printf("This is where the code for mosquitto goes");
			// NOTES:
			// -- The string to be used as output is 'output'
			// -- No need to format anything else
			// myMosq(id, "publish_file_name", host, port);
			// send_message(output);
			break;
	}
}
