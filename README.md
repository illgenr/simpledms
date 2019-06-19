# simpledms
## Dead simple dead man's switch
Send an email at after you stop responding to periodic pings

## Features
- Send an email with a link that confirms you're alive
- Send an email to specified address on confirmation failure
- Adjustable reminder interval
- Adjustable number of reminders

## Installation
### Prereqs
Roswell / SBCL

Sendmail

### Build
Start by installing Sendmail

`sudo apt-get install sendmail`

And configure with

`sudo sendmailconfig`

Next install the Lisp manager Roswell.

Instructions here: https://github.com/roswell/roswell/wiki/Installation

The Ubuntu/Debian package is installed with

```
curl -sOL `curl -s https://api.github.com/repos/roswell/roswell/releases/latest | jq -r '.assets | .[] | select(.name|test("deb$")) | .browser_download_url'`
```

Once Roswell is installed, install Steel Bank Common Lisp with

`$ros install sbcl`

Then cd into your ~/.roswell/local-projects directory and clone the simpledms repo

`git clone https://github.com/illgenr/simpledms.git`

Last, build the project with

`$ros build ./simpledms.ros`

## Getting Started
Use tmux, screen or terminal multiplexer to run the process permanently. 

Create a message with the m command. 

After the message interval has been reached the process will use sendmail to email a message with a token. 

Follow the link in the email to confirm you're alive. 

After the retry count has been hit, the message will be sent to the specified receiver.
