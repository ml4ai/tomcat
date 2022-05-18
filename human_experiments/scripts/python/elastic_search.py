from elasticsearch import Elasticsearch
import argparse
import json


# This script uses the elastic search API to retrieve messages
# of a specific trial. The trial number is collected by another script
# and saved to a file in the experiment directory. When this script
# is called, the stored trial ID is read from the file and informed to this script.


def export_messages(host: str, port: int, trial_id: str, out_filepath: str):
    # The is be performed with pagination to prevent overloading the CPU with multiple threads and
    # because there's a limit of 10000 messages retrieved per request. To preserve the current index and
    # state during subsequent queries for multiple pages, we first create a point in time (PIT) and use that
    # in the page requests. For more details, refer to the elastic search documentation at
    # https://www.elastic.co/guide/en/elasticsearch/reference/current/paginate-search-results.html

    es = Elasticsearch(f"http://{host}:{port}")

    if not es.ping():
        raise Exception("The Elasticsearch cluster is not running.")

    # We keep the PIT alive for 2 minutes. That should be enough to export
    # all the messages from a trial.

    # The body property deserializes the ObjectApiResponse to a dict
    pit = es.open_point_in_time(index="_all", keep_alive="2m").body

    query = {
        "match": {
            "msg.trial_id": trial_id
        }
    }

    sort = [
        {"@timestamp": "asc"},
        {"topic": {"order": "asc", "unmapped_type": "long"}}
    ]

    num_messages = 0
    num_malformed_messages = 0
    last_page = None

    with open(out_filepath, "a") as f:
        while True:
            if last_page is None:
                # The first search does not use search after
                results = es.search(pit=pit, size=1000, sort=sort, query=query, track_total_hits=False)
            else:
                results = es.search(pit=pit, size=1000, sort=sort, query=query, track_total_hits=False,
                                    search_after=last_page)

            if len(results["hits"]["hits"]) == 0:
                break
            for hit in results["hits"]["hits"]:
                try:
                    f.write(json.dumps(hit["_source"]) + "\n")
                    num_messages += 1
                except:
                    num_malformed_messages += 1
            last_page = results['hits']['hits'][-1]["sort"]

    print(f"{num_messages} messages exported.")
    print(f"{num_malformed_messages} malformed messages.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Uses elastic search to extract testbed messages from the testbed and save them to a file.")
    parser.add_argument("--host", help="Elasticsearch host address", default="localhost")
    parser.add_argument("-p", "--port", help="Elasticsearch port", default="9200")
    parser.add_argument("trial_id", help="Unique trial ID")
    parser.add_argument("out_filepath", help="Path to the file with the exported messages")
    args = parser.parse_args()

    export_messages(args.host, args.port, args.trial_id, args.out_filepath)
