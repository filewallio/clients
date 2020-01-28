package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"mime"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/google/logger"

	"github.com/pkg/errors"
)

type Filewall struct {
	apiKey string
	log    *logger.Logger
}

type File struct {
	name    string
	content []byte
}

func NewFilewall(apiKey string, log *logger.Logger) *Filewall {
	return &Filewall{apiKey, log}
}

func (f *Filewall) convert(fi *File) (*File, error) {
	m, err := f.authorize()
	if err != nil {
		return nil, errors.Wrap(err, "authorization")
	}

	err = f.upload(m.Links.Upload, fi.name, fi.content)
	if err != nil {
		return nil, errors.Wrap(err, "upload")
	}

	url, err := f.poll(m.Links.Self)
	if err != nil {
		return nil, errors.Wrap(err, "poll")
	}

	newFi, err := f.download(url)
	if err != nil {
		return nil, errors.Wrap(err, "download")
	}

	return newFi, nil
}

type authResp struct {
	Error string `json:"error"`
	Links struct {
		Self   string `json:"self"`
		Upload string `json:"upload"`
	} `json:"links"`
}

func (f *Filewall) authorize() (*authResp, error) {
	f.log.Info("Authorize")

	client := &http.Client{}
	req, err := http.NewRequest("POST", "https://Filewall.io/api/authorize", nil)
	if err != nil {
		return nil, errors.Wrap(err, "new request")
	}
	req.Header.Add("APIKEY", f.apiKey)
	resp, err := client.Do(req)
	if err != nil {
		return nil, errors.Wrap(err, "do request")
	}
	defer resp.Body.Close()

	var m authResp
	err = json.NewDecoder(resp.Body).Decode(&m)
	if err != nil {
		return nil, errors.Wrap(err, "decode body")
	}
	if m.Error != "" {
		return nil, errors.Wrap(errors.New(m.Error), "check error")
	}
	return &m, nil
}

func (f *Filewall) upload(uploadUrl, filename string, content []byte) error {
	f.log.Info("Upload")

	client := &http.Client{}
	req, err := http.NewRequest("POST", uploadUrl, bytes.NewReader(content))
	if err != nil {
		return errors.Wrap(err, "new request")
	}
	req.Header.Add("filename", filename)
	resp, err := client.Do(req)
	if err != nil {
		return errors.Wrap(err, "do request")
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusAccepted {
		return errors.New("upload failed code isn't 202")
	}

	return nil
}

type pollResp struct {
	Error  string `json:"error"`
	Status string `json:"status"`
	Links  struct {
		Download string `json:"download"`
	} `json:"links"`
}

func (f *Filewall) poll(itemUrl string) (string, error) {
	count := 0
	for count < 100 {
		f.log.Info("Waiting for result")

		client := &http.Client{}
		req, err := http.NewRequest("GET", itemUrl, nil)
		if err != nil {
			continue
		}
		req.Header.Add("APIKEY", f.apiKey)
		resp, err := client.Do(req)
		if err != nil {
			continue
		}
		defer resp.Body.Close()

		var m pollResp
		err = json.NewDecoder(resp.Body).Decode(&m)
		if err != nil {
			return "", errors.Wrap(err, "decode body")
		}

		if m.Error != "" {
			return "", errors.New(m.Error)
		}

		switch m.Status {
		case "waiting", "processing":
			time.Sleep(3 * time.Second)
			continue
		case "failed":
			return "", errors.New("processing failed")
		case "archived":
			return "", errors.New("File archived")
		case "finished":
			if m.Links.Download == "" {
				return "", errors.New("no download url")
			}
			return m.Links.Download, nil
		}

		count++
	}

	return "", errors.New("something went wrong")
}

func (f *Filewall) download(downloadUrl string) (*File, error) {
	f.log.Info("Download results")

	resp, err := http.Get(downloadUrl)
	if err != nil {
		return nil, errors.Wrap(err, "get request")
	}

	if resp.StatusCode != 200 {
		return nil, errors.New("download failed")
	}

	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return nil, errors.Wrap(err, "read body")
	}

	_, disposition, err := mime.ParseMediaType(resp.Header.Get("content-disposition"))

	f.log.Infof("Secured file '%s' (%d byte) downloaded", disposition["filename"], len(b))

	return &File{disposition["filename"], b}, nil
}

func init() {
	flag.Usage = func() {
		fmt.Println("Filewall -v <apikey> <source_file> <replace_source>")
		fmt.Println("apikey: your apikey")
		fmt.Println("source_file: local File")
		flag.PrintDefaults()
	}
}

func main() {
	verbose := flag.Bool("v", false, "print info level logs to stdout")
	stackTrace := flag.Bool("s", false, "print stacktrace on error")
	flag.Parse()

	if contains(flag.Args(), "help") || len(flag.Args()) < 2 {
		flag.Usage()
		os.Exit(1)
	}

	log := logger.Init("Logger", *verbose, false, ioutil.Discard)
	defer log.Close()

	apiKey := flag.Arg(0)
	sourceFile := flag.Arg(1)
	sourceFullPath, err := filepath.Abs(sourceFile)
	if err != nil {
		log.Fatal(err)
	}
	sourceDir := filepath.Dir(sourceFullPath)
	sourceContent, err := ioutil.ReadFile(sourceFile)
	if err != nil {
		if os.IsNotExist(err) {
			log.Fatalf("File %s does not exist", sourceFile)
		}
		log.Fatal(err)
	}

	sourceName := filepath.Base(sourceFile)

	fw := NewFilewall(apiKey, log)
	outputFile, err := fw.convert(&File{sourceName, sourceContent})
	if err != nil {
		if *stackTrace {
			log.Fatalf("%+v", err)
		}
		log.Fatal(err)
	}

	outputFullPath := filepath.Join(sourceDir, outputFile.name)
	i := 1
	for {
		_, err := os.Stat(outputFullPath)
		if err != nil {
			if os.IsNotExist(err) {
				err = ioutil.WriteFile(outputFullPath, outputFile.content, 0644)
				if err != nil {
					log.Fatal(err)
				}
				log.Infof("Secure result: %s", outputFullPath)
				break
			}
			log.Fatal(err)
		}

		ext := filepath.Ext(outputFile.name)
		name := outputFile.name[0 : len(outputFile.name)-len(ext)]
		newFileName := fmt.Sprintf("%s_%d%s", name, i, ext)

		outputFullPath = filepath.Join(sourceDir, newFileName)

		i++
	}
}

func contains(s []string, e string) bool {
	for _, a := range s {
		if a == e {
			return true
		}
	}
	return false
}
