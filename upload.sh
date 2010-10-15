cd _site && rsync -avc --delete-after --exclude=thoughtfolder ./  www-data@gregweber.info:/var/www/blog.gregweber.info/
