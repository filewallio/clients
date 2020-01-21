import json
import requests
import time
import sys
import os
import logging

class Filewall():
    def __init__(self, apikey):
        self.apikey = apikey

    def convert(self, source_filename, source_content):
        response = self._authorize()
        if "error" in response:
            return False, response["error"]

        success = self._upload(response["links"]["upload"], source_filename, source_content)
        if not success:
            return False, "upload_failed"

        success, url_or_msg = self._poll(response["links"]["self"])
        if not success:
            return False, url_or_msg

        success, data_or_msg = self._download(url_or_msg)
        if not success:
            return False, data_or_msg
        return True, data_or_msg

    def _authorize(self):
        logging.info('Authorize')
        try:
            r = requests.post("https://filewall.io/api/authorize", headers={"APIKEY": self.apikey})
            result = json.loads(r.text)
        except:
            result = {"error": "unknown_error"}
        return result

    def _upload(self, upload_url, filename, content):
        logging.info('Upload')
        try:
            r = requests.post(upload_url, content, headers={ "filename": filename})
            return r.status_code == 202
        except:
            return False

    def _poll(self, item_url):
        for _ in range(0, 100):  # poll for max 100x3 seconds
            logging.info('Waiting for result')

            response = {}
            try:
                response = requests.get(item_url, headers={"APIKEY": self.apikey})
                response = json.loads(response.text)
            except:
                pass

            if "error" in response:
                return False, response["error"]

            if "status" in response:
                if response["status"] not in ["waiting", "processing"]:
                    if response["status"] == "failed":
                        return False, "processing_failed"

                    if response["status"] == "archived":
                        return False, "file_archived"

                    if response["status"] == "finished":
                        if "links" in response and "download" in response["links"]:
                            return True, response["links"]["download"]
                        else:
                            return False, "no_download_url"

            time.sleep(3)

    def _download(self, download_url):
        logging.info('Download results')

        try:
            response = requests.get(download_url)
        except:
            return False, "download_failed"

        if response.status_code != 200:
            return False, "download_failed"

        filename = response.headers.get("content-disposition").split('; filename="')[-1].split('"')[0]

        logging.info("Secured file '%s' (%s byte) downloaded" % (filename, len(response.content)))

        return True, (filename, response.content)


def print_help():
    print("filewall.py -v <apikey> <source_file> <replace_source>")
    print("    -v            : verbose (optional)")
    print("    apikey        : your apikey")
    print("    source_file   : local file")


if __name__ == "__main__":
    if "help" in sys.argv or len(sys.argv) < 2:
        print_help()
        exit(1)

    if "-v" in sys.argv:
        logging.basicConfig(level=logging.INFO)

    apikey = sys.argv[-2]
    source_file = sys.argv[-1]

    if not os.path.exists(source_file):
        print("File '%s' does not exist" % source_file)
        exit(1)

    source_path = os.path.dirname(os.path.abspath(source_file))
    source_name = os.path.basename(source_file)
    source_content = open(source_file, "rb").read()

    success, result = Filewall(apikey).convert(source_name, source_content)

    if not success:
        print("Error:", result)
        exit(1)

    filename, content = result

    i = 1
    output_file = os.path.join(source_path, filename)
    while True:
        if not os.path.exists(output_file):
            open(output_file,"wb").write(content)
            print("Secure result: %s" % output_file)
            break
        new_filename = "%s%s%s%s" % (os.path.splitext(filename)[0], "_", i, os.path.splitext(filename)[1])
        output_file = os.path.join(source_path, new_filename)
        i+=1

