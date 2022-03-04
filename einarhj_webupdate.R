devtools::build_readme() # not working on windose
pkgdown::build_site()  # have to run this on windose
# run this on linux
system("rm -rf  /net/www/export/home/hafri/einarhj/public_html/xe/*")
system("cp -r docs/* /net/www/export/home/hafri/einarhj/public_html/xe/.")
system("chmod -R a+rX /net/www/export/home/hafri/einarhj/public_html/xe")
