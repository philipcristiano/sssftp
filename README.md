# SSSFTPD

An SFTP server that stores files on S3.


## Setup

* Create a key with `ssh-keygen -t rsa -f /etc/ssh/ssh_host_rsa_key`

* Add your public key to `~/.ssh/authorized_users`

* Run the server `make deps app shell

### AWS

Add standard `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` environmental variables in order to access S3.

Set `AWS_BUCKET` for the bucket to use for SSH keys and uploaded files.


#### Users

Users go into `$AWS_BUCKET/credentials/$USER/id_rsa.pub`

Create `$AWS_BUCKET/uploads/$USER` directory for their specific files.


## Configuration

Port (ENVVAR `PORT`, default 8989) - Port to listen for SSH connections
