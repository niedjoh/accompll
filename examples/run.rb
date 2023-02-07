#!/usr/bin/ruby

def natsort(a)
  a.sort_by {|path|
    b = path.split(/(\d+)/).collect {|x| x.match(/\d+/) ? x.to_i : x }
    b && b[0].match(/\d+/) ? b : [-1] + b
  }
end

CONFIG_REGEX = /\A\s*(\S+)\s*=\s*([^ \t\r\n;]*);?\s*\z/

def read_status file
  h = {}
  open(file) {|f| 
    f.readlines.each {|s| 
      a = s.scan(CONFIG_REGEX)[0]
      h[a[0]] = a[1] if a != nil && a.length == 2
    }
  }
  h
end

def skip status_file, timeout, skip_error
  if FileTest.exist? status_file
    h = read_status status_file
    h["status"] == "YES" ||
    h["status"] == "NO" ||
    h["status"] == "MAYBE" ||
    (h["status"] == "ERROR" && skip_error)||
    (h["status"] == "TIMEOUT" && (print "-- #{h["timeout"].to_i} (old) vs #{timeout}\n"; h["timeout"].to_i >= timeout))
  else
    false
  end
end

skip_error = ARGV.include?("-skip-error")
ARGV.delete("-skip-error")

unless ARGV.size == 2 || ARGV.size == 3
  puts "run <timeout> <command> [directory]"
  exit 1
end

timeout = ARGV[0].to_i
command = ARGV[1]
dir = ARGV[2] ? ARGV[2] : File.basename(command, ".*")
p dir

Dir.mkdir dir unless FileTest.exist? dir

files = natsort(Dir.glob("*.{srs,trs,tptp,p}"))
#files = Dir.glob("*.[st]rs").sort_by {|x| FileTest.size(x) }

count = 1

for file in files
  base = File.dirname(file) + "/" + File.basename(file, ".*")
  subdir = (File.dirname(file) == ".") ? 
    dir : (dir + "/" + File.dirname(file))
  system "mkdir -p #{subdir}" unless FileTest.exist? subdir
  status_file = "#{dir}/#{base}.status"
  txt_file = "#{dir}/#{base}.txt"
  timeout_flag = false
  if skip status_file, timeout, skip_error
    puts "#{count}/#{files.size}:  #{base}.status exists.  skipped"
  else 
    time0 = Time.now
    if FileTest.exists?(command) && ! FileTest.directory?(command)
      cmd = "./#{command} #{file} #{timeout.to_i}"
    else
      cmd = "timeout #{timeout.to_i}s #{command} #{file}"
    end
    puts "#{count}/#{files.size}:  #{cmd}"
    txt = `#{cmd}`
    time = Time.now - time0
    open(txt_file, "w+") {|f| f << txt }
#    status = (txt =~ /^(YES|NO|MAYBE|TIMEOUT)/) ? $1 : 
#             (time >= timeout.to_i ? "TIMEOUT" : "ERROR")
    case txt
    when /^(YES|NO|MAYBE|TIMEOUT)/
      status = $1
    when /^(%|#) SZS status (Success|SAT|Satisfiable)/
      status = "YES"
    when /^(%|#) SZS status (UNSAT|Unsatisfiable)/
      status = "NO"
    else
      status = time >= timeout.to_i ? "TIMEOUT" : "ERROR"
    end
    puts status
    open(status_file, "w+") {|f| 
      f.puts "timeout = #{timeout}"
      f.puts "time = #{format("%.03f", (status == "TIMEOUT" ? timeout : time))}"
      f.puts "status = #{status}"
    }
  end
  count += 1
end
