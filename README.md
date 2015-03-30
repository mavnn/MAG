Welcome!
========

This code is nearing completion as a set of demo code, but it's not quite there yet: check back for a last few updates to the UI over the next week or two.

In the mean time, you're probably here because you attended my talk at LambdaCon. Hope you enjoyed it, here's a few tips to get you started!

1. This code has some external dependencies managed by Paket. Run restore.bat first to grab those.
2. You should now have a working, building Visual Studio solution. If you want to play with the code a bit, I suggest starting with the ``Script.fsx`` file in the Scripts folder of the main ``MAG`` project. This will allow you to get a feel for the overall shape of the code.
3. You can also run the ``MAG.Server`` project as a command line app, which will spin up a self-hosted webservice. This service relies on finding [EventStore](http://geteventstore.com) at a specific internal IP address. If you have [Vagrant](https://www.vagrantup.com/) installed then running ``vagrant up`` in the main solution directory will set up a virtual machine with EventStore running for you. This will be a VirtualBox machine with a host only network connection.
4. The main api endpoints for the webserver work, but are undocumented at the moment. The website also contains a set of views to show what information is available to different players of the game. Unfortunately, the form it displays when it is that players turn to act does not yet work - hopefully this will be sorted shortly. You will currently need to drive the webserver via direct http POST requests...
