    Title: Racket 6.9 and Windows 10 Creators Update
    Date: 2017-05-26T17:00:28
    Tags: racket, windows, bugs, by Leif Andersen

[Racket][racket] 6.9 was released in April and it has been smooth sailing for
many people. However, some people using the [Windows 10 Creators Update][WCU] have
been experiencing [crashes][crash], not just for Racket, but for the whole
operating system. This is due to a bug in Windows. We have contacted Microsoft;
they have classified the bug as (1) a stack overflow and (2) not a security
hazard, and intend to add a fix in a future version of Windows.

The next version of Racket will include a patch to help avoid triggering the bug.
Until then, one work-around is to run Racket in a virtual machine (VM).
This blog post is a step-by-step guide on how to install a VM for Racket.

A VirtualBox image with Racket preinstalled can be downloaded here:

- <https://github.com/nuprl/website/releases/download/racket69vm/Racket_6_9.ova>

The username and password for this machine are both `racket`.

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

4. After import finishes, you should now see your new VM in the list on the left of the
VirtualBox manager. Select it and hit `Start`. Once started up, you will see
DrRacket and Firefox on the VM's desktop.

[racket]: http://racket-lang.org/
[vm]: https://github.com/nuprl/website/releases/download/racket69vm/Racket_6_9.ova
[crash]: https://github.com/racket/racket/issues/1671
[virtualbox]: https://www.virtualbox.org/
[vboxdownload]: https://www.virtualbox.org/wiki/Downloads
[WCU]: https://blogs.windows.com/windowsexperience/2017/04/11/whats-new-in-the-windows-10-creators-update/
