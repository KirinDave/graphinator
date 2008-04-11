require 'rubygems'
require 'rake'

task :default do
  sh "mzc -z -d lib/ src/*.ss"
end

task :console do
  puts "Git-Graphinator Console - All Modules Loaded\n"
  exec "mzscheme -m -S ./src -t src/*.ss"
end

task :clean do
  sh "git clean -xf"
end
