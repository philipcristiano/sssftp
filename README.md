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

Configuration is provided by a config file defining the SFTP servers to run

```
[{sssftp, [
    {servers, [
        [{port, 8989}, {aws_bucket, "sssftpd-test"}]
    ]}]}].
```

Multiple port/bucket combinations can be used.

In addition to `port` and `aws_bucket` are `role` and `external_id`. `role` and `external_id` will be used if defined to assume a role from another account.

