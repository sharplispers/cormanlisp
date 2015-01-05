;;;;	-------------------------------
;;;;	Copyright (c) Corman Technologies Inc.
;;;;	See LICENSE.txt for license information.
;;;;	-------------------------------
;;;;
;;;;	File:				insults.lisp
;;;;	Contents:			Elizabethan insult generator.
;;;;
;;;;	Insults variously attributed to:
;;;;						a) William Shakespeare
;;;;						b) A fellow priest who was the Vicar of a
;;;; 						neighbouring parish when I worked in the beautiful Waikato 
;;;;						District
;;;;						c) Matthew A. Lecher  (mal6315@cs.rit.edu)
;;;;					    d) Jerry Maguire, who teaches English at Center Grove High School
;;;;
;;;;	Program creator:	Roger Corman
;;;;

(defvar column1 nil)
(defvar column2 nil)
(defvar column3 nil)
(let ((words '(
;;;			1						 2						 3   
		 artless                 base-court              apple-john
         bawdy                   bat-fowling             baggage
         beslubbering            beef-witted             barnacle
         bootless                beetle-headed           bladder
         churlish                boil-brained            boar-pig
         cockered                clapper-clawed          bugbear
         clouted                 clay-brained            bum-bailey
         craven                  common-kissing          canker-blossom
         currish                 crook-pated             clack-dish
         dankish                 dismal-dreaming         clotpole
         dissembling             dizzy-eyed              coxcomb
         droning                 doghearted              codpiece
         errant                  dread-bolted            death-token
         fawning                 earth-vexing            dewberry
         fobbing                 elf-skinned             flap-dragon
         froward                 fat-kidneyed            flax-wench
         frothy                  fen-sucked              flirt-gill
         gleeking                flap-mouthed            foot-licker
         goatish                 fly-bitten              fustilarian
         gorbellied              folly-fallen            giglet
         fool-born               gudgeon				 ()
         infectious              full-gorged             haggard
         jarring                 guts-griping            harpy
         loggerheaded            half-faced              hedge-pig
         lumpish                 hasty-witted            horn-beast
         mammering               hedge-born              hugger-mugger
         mangled                 hell-hated              joithead
         mewling                 idle-headed             lewdster
         paunchy                 ill-breeding            lout
         ill-nurtured            maggot-pie				 ()
         puking                  knotty-pated            malt-worm
         puny                    milk-livered            mammet
         qualling                motley-minded           measle
         rank                    onion-eyed              minnow
         reeky                   plume-plucked           miscreant
         roguish                 pottle-deep             moldwarp
         ruttish                 pox-marked              mumble-news
         saucy                   reeling-ripe            nut-hook
         spleeny                 rough-hewn              pigeon-egg
         spongy                  rude-growing            pignut
         rump-fed                puttock				 ()
         tottering               shard-borne             pumpion
         unmuzzled               sheep-biting            ratsbane
         vain                    spur-galled             scut
         venomed                 swag-bellied            skainsmate
         tardy-gaited            strumpet				 ()
         warped                  tickle-brained          varlot
         wayward                 toad-spotted            vassal
         weedy                   unchin-snouted          whey-face
         yeasty                  weather-bitten          wagtail
	)))
	(do ((x words (cdddr x))) 
		((null x))
		(push (first x) column1)
		(push (second x) column2)
		(push (third x) column3)))

(setq column1 (nreverse column1))
(setq column2 (nreverse column2))
(setq column3 (remove nil (nreverse column3)))

(defun random-elt (list)
	(nth (random (length list)) list))

(defun hurl-insult ()
	(format t "Thou ~A ~A ~A!~%"
		(random-elt column1)
		(random-elt column2)
		(random-elt column3)))

(defun hurl-insults (number-of-insults)
	(dotimes (i number-of-insults)
		(hurl-insult)))

(do ((done nil))
	(done "Now I feel better!")
	(hurl-insults 3)
	(setq done (y-or-n-p "Had enough?")))
