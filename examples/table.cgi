#!/usr/bin/ruby

require 'uri'
require 'cgi'
require 'erb'

$statuses = ["YES","NO","MAYBE","TIMEOUT","ERROR"]

DPLUS = /\d+/
GROUPED_DPLUS = /(\d+)/

def natsort(a)
  a.sort_by {|path|
    b = path.split(GROUPED_DPLUS).collect {|x| x.match(DPLUS) ? x.to_i : x }
    b && b[0].match(DPLUS) ? b : [-1] + b
  }
end

def uri_encode(input)
  URI::DEFAULT_PARSER.escape(input, URI::UNSAFE)
end

CONFIG_REGEX = /\A\s*(\S+)\s*=\s*([^ \t\r\n;]*);?\s*\z/

def read_config(file)
  return nil unless FileTest.exist?(file)
  h = {}
  for line in IO.readlines(file)
    if line =~ CONFIG_REGEX
      h[$1] = $2
    end
  end
  h
end


def read_status(file, timeout)
  return nil unless FileTest.exist?(file)
  h = read_config(file)
  h['time'] = h['time'].to_f
  if timeout && h['time'] > timeout
    h['status']  = 'TIMEOUT'
    h['time']    = timeout
  end
  h['outfile'] = find_ext(File.join([File.dirname(file), File.basename(file, ".*")]), [".html", ".txt"])
  h
end

def ahref(href, s) 
  href ?  "<a href=\"#{uri_encode(href)}\">#{s}</a>" : s
end

def find_ext(file, exts) 
  exts.map {|ext| file + ext}.find {|path| FileTest.exist?(path) }
end

def read_statuses(path, tools, statuses, timeout)
  # reading path/tool/file.status
  time = {}
  count = {}
  for tool in tools
    time[tool] = {}
    count[tool] = {}
    for status in statuses
      count[tool][status] = 0
      time[tool][status] = 0.0
    end
  end
  files = natsort(Dir.glob("*.{srs,trs,tptp,p}", base: path))
  results = {}
  for file in files
    status_file = File.basename(file, ".*") + ".status"
    results[file] = {}
    for tool in tools
      results[file][tool] = read_status(File.join([path,tool,status_file]), timeout)
      if results[file][tool] != nil
        status = results[file][tool]['status'] 
        if statuses.include?(status)
          count[tool][status] += 1
          time[tool][status] += results[file][tool]['time']
        end
      end
    end
  end
  [results, count, time]
end

def glob_dirs(path, base: nil)
  dir = path + "/"
  natsort(Dir.glob(dir, base: base).map {|s| s[-1] == '/' ? s[0..-2] : s })
end

def subdirectories(patterns)
  for pattern in patterns
    dirs = glob_dirs(File.dirname(pattern))
    subdirs = {}
    for dir in dirs
      subdirs[dir] = glob_dirs(File.basename(pattern), base: dir)
    end
  end
  subdirs
end

directories_txt = "directories.txt"
config = {}
if File.exist?(directories_txt)
  config[:dir] = nil
  config[:patterns] = IO.readlines(directories_txt).map {|s| s.chomp }
  subdirs = subdirectories(config[:patterns])
else
  config = { patterns: ["."], dir: "." }
  subdirs = {"." => "."}
end

cgi = CGI.new
if cgi.params['dir'] && cgi.params['dir'][0]
  config[:dir] = File.join(cgi.params['dir'][0].split(','))
end

if config[:dir]
  statuses = $statuses
  timeout = nil
  tools = glob_dirs('*', base: config[:dir])
  table_cfg = File.join(config[:dir], "table.cfg")
  if File.exist?(table_cfg)
    config.merge!(read_config(table_cfg))
    statuses = config['statuses'].scan(/\w+/) if config['statuses']
    timeout = config['timeout'].to_i if config['timeout'] && config['timeout'] != ""
    tools = config['tools'].split(',').map {|s| s.strip } if config['tools']
  end
  if cgi.params['tools'] && cgi.params['tools'][0]
    tools = cgi.params['tools'][0].split(',')
  end
  if cgi.params['timeout'] && cgi.params['timeout'][0]
    s = cgi.params['timeout'][0]
    timeout = s =~ DPLUS ? s.to_i : nil
  end
  results, count, time = *read_statuses(config[:dir], tools, statuses, timeout)
  header_html = File.join(config[:dir], "header.html")
  header = FileTest.exist?(header_html) ? IO.read(header_html) : ""
end

rhtml = File.join([File.dirname(File.realpath(__FILE__)), File.basename(__FILE__, ".*") + ".rhtml"])

ERB.new(IO.read(rhtml), nil, '%').run
