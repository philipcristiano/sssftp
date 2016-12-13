# SSSFTPD

An SFTP server that stores files on S3.


## Setup

* Create a key with `ssh-keygen -t rsa -f /etc/ssh/ssh_host_rsa_key`

* Add your public key to `~/.ssh/authorized_users`

* Run the server `make deps app shell
