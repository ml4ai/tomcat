# Update inspect-data.json
datasette inspect /space/$USER/tomcat/tomcat.db > inspect-data.json
git add inspect-data.json
git commit -m "Updating inspect-data.json"
git push
