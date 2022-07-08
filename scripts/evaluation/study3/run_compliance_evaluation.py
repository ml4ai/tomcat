import argparse
from glob import glob

from common import metadata_message_generator

if __name__ == "__main__":
    # parsing program arguments
    parser = argparse.ArgumentParser(description="Evaluate compliance in trials")
    parser.add_argument(
        "--data_dir",
        help="Directory containing .metadata files",
        default="/media/mule/projects/tomcat/protected/study-3_2022",
    )
    args = parser.parse_args()

    for filepath in glob(args.data_dir + "/*T00*UAZ*.metadata"):
        for message in metadata_message_generator(filepath):
            print(message["header"]["timestamp"])
