    Title: Racket 6.9 and Windows 10 Creative Update
    Date: 2017-05-26T17:00:28
    Tags: racket, windows, bugs, by Leif Andersen

[Racket][racket] 6.9 was released in April and it has been smooth sailing for
many people. However, some people using the Windows 10 Creative Update have
been experiencing [crashes][crash], not just for Racket, but for the whole
operating system. This is a caused by a bug Windows, not Racket. We have
contacted Microsoft but they have not yet fixed their operating system.  The
next version of Racket will have a patch to work around this Windows bug. But
until the next version is released, there is a workaround for some users: run
Racket in a VM.  This blog post gives you a step by step guide on how to
install a VM for Racket in Windows.

[A VirtualBox image with Racket preinstalled can be downloaded here.][vm] The
rest of this blog post is a tutorial on how to install this image.

**Update** The username and password for this machine are both `racket`.

<!-- more -->

1. The first thing you need to install is virtualization software. In principle
it doesn't matter what you install, but for this tutorial, we will use
[VirtualBox][virtualbox]. Go to their [downloads][vboxdownload] page and
download and install the version for your platform (most likely Windows).

2. Once installed, you need to download a virtual image and install Racket on
it. We have prepared an image that comes with Racket pre-installed, which [you
can download here][vm]. The rest of this tutorial will assume you are using
this image.

3. Start up VirtualBox and import the virtual machine. You can do this by
clicking on `File -> Import Appliance`. In the dialog, select the image you
downloaded and hit `Continue`. The next window lets you change the specs for
your virtual machine. Feel free to make any changes you want, but the defaults
work fine for this image. Once you are satisfied click `Import`.

4. That's it. You should now see your new VM in the list on the left of the
VirtualBox manager. Select it and hit `Start`. Once started up, you will see
DrRacket and Firefox on the VM's desktop.

[racket]: http://racket-lang.org/
[vm]: https://github.com/nuprl/website/releases/download/racket69vm/Racket_6_9.ova
[crash]: https://github.com/racket/racket/issues/1671
[virtualbox]: https://www.virtualbox.org/
[vboxdownload]: https://www.virtualbox.org/wiki/Downloads
