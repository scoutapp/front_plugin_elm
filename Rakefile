elm_main = "src/Main.elm"
elm_target = "elm.js"

html_template = "index.template"
html_target = "index.html"

desc "Compile"
task :compile do
  system("elm-make #{elm_main} --output #{elm_target}")
  system("cp #{html_template} #{html_target}")
end
