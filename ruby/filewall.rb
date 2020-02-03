require 'httparty'
require 'optparse'
require 'logger'

class Filewall
  include HTTParty

  def initialize(api_key, log)
    @api_key = api_key
    @log = log
  end

  def convert(source_filename, source_content)
    response = self.authorize
    if response.key? :error
      return [false, response[:error]]
    end

    success = self.upload response[:links][:upload], source_filename, source_content
    unless success
      return [false, 'upload_failed']
    end

    success, url_or_msg = self.poll response[:links][:self]
    unless success
      return [false, url_or_msg]
    end

    success, data_or_msg = self.download url_or_msg
    unless success
      return [false, data_or_msg]
    end

    [true, data_or_msg]
  end

  protected

  def authorize
    @log.info 'Authorize'
    begin
      response = self.class.post "https://filewall.io/api/authorize", {headers: {APIKEY: @api_key}, format: :plain}
      result = JSON.parse response, symbolize_names: true
    rescue
      result = {error: 'unknown error'}
    end
    result
  end

  def upload(upload_url, filename, content)
    @log.info 'Upload'
    begin
      response = self.class.post upload_url, {body: content, headers: {filename: filename}}
      return response.code == 202
    rescue
      return false
    end
  end

  def poll(item_url)
    (0..100).step(1) do |n|
      @log.info 'Waiting for result'

      begin
        response = self.class.get item_url, {headers: {apikey: @api_key}, format: :plain}
        response = JSON.parse response, symbolize_names: true
      end

      if response.has_key?(:error)
        return [false, response[:error]]
      end

      if response.key? :status
        case response[:status]
        when 'failed'
          return [false, 'processing_failed']
        when 'archived'
          return [false, 'file_archived']
        when 'finished'
          if response.key? :links and response[:links].key? :download
            return [true, response[:links][:download]]
          else
            return [false, 'no_download_url']
          end
        end
      end

      sleep(3)

      [false, 'something went wrong']
    end
  end

  def download(download_url)
    @log.info 'Download results'
    begin
      response = self.class.get download_url
    rescue
      [false, 'download_failed']
    end

    if response.code != 200
      [false, 'download_failed']
    end

    filename = response.headers['content-disposition'].split('; filename="')[-1].split('"')[0]

    @log.info "Secured file '%s' (%s byte) downloaded" % [filename, response.body.length]

    [true, [filename, response.body]]
  end
end

if __FILE__ == $0
  log = Logger.new(STDOUT)

  def print_help
    puts "filewall.py -v <apikey> <source_file> <replace_source>"
    puts "    -v            : verbose (optional)"
    puts "    apikey        : your apikey"
    puts "    source_file   : local file"
  end

  if ARGV.include? 'help' or ARGV.length < 2
    print_help
  end

  if ARGV.include? '-v'
    log.level = Logger::INFO
  end

  api_key = ARGV[-2]
  source_file = ARGV[-1]

  unless File.exist? source_file
    log.fatal "File '%s' does not exist" % source_file
  end

  source_path = File.dirname(File.absolute_path(source_file))
  source_name = File.basename(source_file)
  source_content = File.open(source_file).read

  success, result = Filewall.new(api_key, log).convert source_name, source_content

  unless success
    log.fatal result
    exit(1)
  end

  filename, content = result
  output_file = File.join source_path, filename

  i = 1
  while true
    unless File.exist? output_file
      File.write output_file, content
      log.info "Secure result: %s" % output_file
      break
    end

    ext = File.extname filename
    name = filename[0..filename.length-ext.length-1]
    new_filename = "%s_%s%s" % [name, i,  ext]
    output_file = File.join source_path, new_filename

    i += 1
  end
end







