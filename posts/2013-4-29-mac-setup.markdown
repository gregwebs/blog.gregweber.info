---
title: "A Linux developer on a Mac"
---

I have used Windows, Mac & Linux. I don't like Windows at all, although I don't have much experience with the latest versions of Windows.
I much prefer Linux to Mac. However, Linux driver support sucks: I have always had issues with my laptops. Mac provides solid hardware plus you can run Mac-only software.

My end goal is for a work environment as nice as what I have on Linux.
I have 3 main issues with Mac (that I think all hold for Windows):

* hard to install libraries (no package manager)
* no autocopy (copy on highlight)
* broken window management (I want my tiling window manager!)


## Improving the screen

Time for another Mac issue: they push their high-gloss displays.
This issue is a clear representation of a mindset that values eye candy over functionality.
You can get a glare reducer, but it is best to order the matte display.

I install [Flux](http://stereopsis.com/flux/) so my screen doesn't keep me awake at night.  
I uncheck `Slightly dim the display when using this power source` in the Energy Saver Battery section.


## Hard to install libraries

Installing a GUI application tends to be easy since everything is bundled together, but developers need libraries and command line tools.

I am using homebrew to install things, but as little as possible. I hate setting up a dev environment on the Mac. I know how to find Ubuntu packages quickly and efficiently. More importantly, I always work on projects that are deployed to Linux. So why not just keep the project on Linux?


### Install on Linux, use vagrant

There is a solution: use a virtual machine. The *code* on the VM is mounted from Mac folders. It is easy to share some of the filesystem with a VM. So the plan is to do *all* development installation on a headless Linux VM. Then I only need the basics installed on my Mac. This VM can be shared among developers and also used in (at least to test) deployment.

I am using [vagrant](http://vagrantup.com/docs/boxes.html). Vagrant helps take some of the tedium out of maintaining a VM.

I had a *huge* problem though. My VM was unbearably slow. I realized that it was actually able to run locally just fine, but that using the browser to hit the VM was causing the issue. A lot of googling lead to [this blog post](http://www.jedi.be/blog/2011/03/28/using-vagrant-as-a-team/) which led to this command to fix the DNS issue on my Ubuntu VM:

    sudo apt-get remove libavahi-common3

The basic goal is to remove mdns DNS lookup. Chrome was working, but I saw issues on Firefox. I disabled IPV6 on Firefox, and it was working. But the problem came back again on all browsers. I tried switching Vagrant from host-only networking to bridged, but that only improved things tempoarily once again.

Finally I found [this tip](http://nowfromhome.com/virtualbox-slow-network-from-windows-host-to-linux-guest/). There is a webrick setting to fix dns lookup. Instead of setting that I am now using unicorn.

This kind of stuff is a big failure for open source. Multiple people have already gone through the exact same problems as myself. However, instead of adding to the project documentation they just blog about it. The result is days of low productivity for me. Blogging should be for temporary information, extremely specific or overly broad information or unofficial thoughts. When you figure out something generally useful for a specific community, it needs to be shared through an official channel of the project. I added the above information to the project documentation and sent a pull request, and it only took a few minutes.


#### Stale NFS file handles

I had one more problem: stale NFS file handles. I had to wait (perhaps a second) for files to sync over after saving. One solution is to put the editor on the server. This works well for a vim user like myself since vim is light-weight and runs well in a terminal.

I do need all my vim plugins, so I just mapped an nfs share of my .vim folder.

    config.vm.share_folder("vim-plugins", "/home/vagrant/.vim", "/Users/#{env['USER']}/Dropbox/.vim", :nfs => true)

I put my .vimrc file in my .vim folder.
My `~/.vimrc` file everywhere now contain just: `source ~/.vim/.vimrc`

Unfortunately this solution would not satisfy users of GUI editors. The regular file system (not nfs) option probably syncs faster but has had its own issues in the past (maybe they are solved now).


#### Personal customization

A Vagrant VM should be sharable across the team and emulate production, but I want to be able to run programs on it that are convenient for me when developing and not used on the deployment. I am following the pattern above with vim to mount my zsh customizations. But I need the zsh binary, so I am just going to install it on the system. I make note of personalized installs and put them in an install/uninstall script so that if I want to share or refresh the VM I can with minimal hassle.


## Setting up a Linux Desktop.

I am also running an Ubuntu Desktop to separate my personal usage from work, although I may attempt to create a linux VM for work in the future also.

I figured out how to make auto-copy convenient. I am using the program BetterTouchTool to map 3-finger press globally to middle-click (auto-copy paste on Linux), and it works in my VM. I map 3-finger press for iTerm and Terminal to Apple-V (paste)
I also use BetterTouchTool's window snapping functionality (Under Action Settings) and I turn on Resize when holding down the Fn key.

I am using XMonad with [unity-2d](http://markhansen.co.nz/xmonad-ubuntu-oneiric/), which means I finally have some sane window management!


## Development programs

These are all I need for my development:
* XCode, available from the Mac app store
  * download [Command Line Tools for Xcode](https://developer.apple.com/downloads).
* [git](http://help.github.com/mac-set-up-git/)
* chrome with vimium or firefox with [vimperator](http://vimperator.org/vimperator)
* IntelliJ Community Edition

It is difficult to avoid installing the programming languages that I use on my Mac because editors need to reach them to have enchanced functionality. However, I am still avoiding installing library dependencies on my Mac, instead doing that on the VM. The issue of using Scala in an IDE did make me run the build on my Mac, but I did have the code base communicating with databases installed on the vagrant VM.


### configuration

I keep all my dotfiles in my Dropbox: that way all my computers have the latest configuration available.. When I get on a new machine, I first install Dropbox and then I run a script that creates symlinks for all of them.

I also keep my passwords in a Keepass database in Dropbox. There is a version of keepass that works on every OS. [Here is the one for mac](http://www.keepassx.org/downloads).

I set my shell to zsh.

    chsh -s /bin/zsh

I use zsh primarily for its extended globbing operators, including `**/*`.
Zsh seems to be gaining popularity because of a [project to share configuration files](github.com/robbyrussell/oh-my-zsh), I plan on trying it out soon.


### Mac System Prefernces configuration:

The lack of a control key bugs the heck out of me. I use [this free program](http://www.macupdate.com/app/mac/25141/keyremap4macbook) to remap Right Alt to Right Control and Right Command to Right Alt. I also added [KeyRemap4MacBook](http://pqrs.org/macosx/keyremap4macbook/extra.html#t1) to change Caps Lock to Esc.

* I hide the dock and make it magnify rather than waste a lot of great screen real estate.
* I uncheck the keyboard shortcuts for Show Desktop & Show Dashboard (F12 opens firebug)


### Iterm2

There are fewer reasons to use Iterm on Mac. Terminal.app now supports tabs, splits, and 256 colors. But there is still one crucial feature missing: auto-copy. This made me ditch Terminal in favor of [iTerm2](http://www.iterm2.com/#/section/home)

Since I stare at my terminal all day long, I want an optimal color scheme. I like dark terminals, but I find light-colored is easier to read in sunlight. The [solarized](http://ethanschoonover.com/solarized) light color scheme works well. There is iterm support and support for many other programs. After downloading and unzipping, open the files in iterm2-colors-solarized. I increase the terminal font-size when outdoors.


### Vim

I use the syntastic plugin, which gives you syntax/compiler errors on save. I switched from command-t to [ctrip](http://kien.github.com/ctrlp.vim/), which is easier to install because it is pure vimscript.

The other most important Vim plugin I use are NerdTree (file explorer), powerline (status bar), and vundle (plugin manager). With these, syntastic, and other language-specific plugins you are getting closer to an IDE, but still feel very light-weight and can work in a terminal.

If you are a new or experienced vim user, you should definitely check out the [janus](https://github.com/carlhuda/janus) distribution which includes some of the above plugins and also some others I haven't had a chance to check out yet.


## Mac window management is broken

When I click on a chrome downoad, it is invisible because it shows up behind the chrome window. I can deal with bugs, but I am used to using XMonad, a tiling (automatic layout) window manager on Linux. It is extraordinarily painful to be back to doing manual resizing. From my reserach there is no solution to this problem on the Mac. There is TylerWM, but it doesn't work for many common programs. I am exploring this tool instead: https://github.com/jigish/slate
You setup pre-defined layouts for different monitor setups. This should work out ok because most of the time I use a tiling window manager it is with the same windows in the same position.

There are a whole lot of programs that make important improvements to Mac windowing, here is what I found so far that I haven't ruled out:

http://itunes.apple.com/ne/app/tri/id454865069?mt=12
moom & divvy let you save positions
http://mizage.com/divvy/
http://itunes.apple.com/us/app/spectacle/id487069743o
http://itunes.apple.com/us/app/bettersnaptool/id417375580?mt=12
http://www.irradiatedsoftware.com/sizeup/
http://manytricks.com/moom/
http://www.macupdate.com/app/mac/33629/optimal-layout

QuickSilver/RSI

TMux/(ITerm + DVTI)
