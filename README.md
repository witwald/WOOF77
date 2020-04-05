# WOOF77
The Fortran program WOOF77 can be used as a numerical evaluator of vented, sealed and infinite baffle
direct radiator electrodynamic loudspeaker systems. It evaluates the absolute low-frequency response
and electrical impedance as functions of frequency. The program listing, typical input data, and an
example of the program’s output are provided.

## REFERENCE

Lampton, Michael L. (1972). Program WOOF: A Numerical
Evaluator of Loudspeaker Systems. IEEE Transactions on
Audio and Electroacoustics, Vol. AU-20, No. 5, December
1972, pages 364-366.

## ABSTRACT FROM ABOVE JOURNAL PAPER

A Fortran language computer program is described that evaluates
the absolute low-frequency response and electrical impedance
functions of frequency for direct radiator electrodynamic
loudspeakers mounted in vented, unvented, or infinite baffle
enclosures. The program is intended to be used interactively,
with the engineer serving to suggest modifications of a design
and the program providing the modified response plots. The
program listing, typical input data, and an example of the
program’s output are shown.

## NOTES

The program WOOF77 is a modified version of program WOOF. The
main changes relate to WOOF's conversion to use Fortran 77.
The use of Holleriths has been eliminated and hopefully improves
clarity for the user. The plotting section has also been modified
to provide greater frequency resolution (1 Hz rather than 2 Hz).
The impedance plot now covers a range from 0 to 120 ohms, rather
than 0 to 150 ohms. The program now outputs the absolute sound
pressure level output in dB, based on the computed driver
efficiency. After the frequency reponse plots, the SPL and
impedance responses are listed for the user to study if desired.
