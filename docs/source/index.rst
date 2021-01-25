.. mosdef_cp2k_writer documentation master file, created by
   sphinx-quickstart on Sat Oct  3 22:59:38 2020.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to mosdef_cp2k_writer's documentation!
==============================================

Overview
^^^^^^^^

mosdef_cp2k_writer is designed to be a simple way to programatically work with CP2K (https://www.cp2k.org/) input files. It does not
interact directly with any of the CP2K source code, instead working solely with a lightweight simulation input object. This
object writes the input file to text and tracks changes to input parameters. In this way, all changes to variables along the 
course of the simulation can be recorded (e.g. when switching from an equilibration period to production period), enhancing
reproducibility. For all supported input parameters, we have also implemented type-checking. Currently, not all CP2K input
sections have been added. We hope to support more input sections in the future.

.. toctree::
   :maxdepth: 2
   :caption: Contents:
   :numbered:
   
   install.rst
   example.rst
   supported_sections.rst



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
