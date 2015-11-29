Kouluarvosanatietokanta
=======================


Esittely
--------

_Kouluarvosanatietokanta_ _(Koas)_ on tietokantaohjelma koulun tai
oppilaitoksen opettajalle. Tietokantaan voi syöttää oppilaita,
koulusuorituksia sekä arvosanoja ja mahdollisia lisätietoja.
Tietokannasta voi hakea tietoja esimerkiksi seuraavilla tavoilla:

  * tietyn oppilaan kaikki suoritukset ja arvosanat
  * tietyn suorituksen arvosanat koko opetusryhmältä
  * opetusryhmän kaikki arvosanat koottuna.

Ohjelma myös laskee suoritusten keskiarvot automaattisesti ja hyödyntää
laskemisessa käyttäjän määrittämiä painokertoimia.

Ohjelmassa on tekstipohjainen käyttöliittymä ja myös tulosteet ovat
tekstimuotoisia taulukoita. Tulosteet saa myös [Wilman][Wilma] viestiin
sopivassa taulukkomuodossa, ja näin tulosteen voi kopioida suoraan
Wilma-viestiin.

[Wilma]: http://www.starsoft.fi/public/?q=fi/node/54


Asentaminen
-----------

_Kouluarvosanatietokanta_ toimii GNU/Linux-käyttöjärjestelmissä ja
luultavasti myös Mac OS X -käyttöjärjestelmissä. Ohjelman kääntäminen
lähdekoodista vaatii [SBCL][]-nimisen Common Lisp -toteutuksen. Lisäksi
asennettuna täytyy olla kehittäjäversio [Readline][]- ja
[SQLite3][]-kirjastosta. Esimerkiksi [Debian GNU/Linux][Debian]
-käyttöjärjestelmässä edellä mainitut ovat asennuspaketeissa nimeltä
`sbcl`, `libreadline6-dev` ja `libsqlite3-dev`.

Ensimmäisellä kerralla kääntämisen yhteydessä käytetään `wget`-ohjelmaa,
jolla haetaan internetistä [Quicklisp][QL], jos sitä ei ole jo valmiiksi
asennettuna. Quicklispin avulla ladataan internetistä automaattisesti
eräitä Common Lisp -kirjastoja.

Ohjelma käännetään komennolla `make` ja asennetaan komennolla `make
install`. Ohjelmatiedosto on nimeltään `koas`, ja se asentuu oletuksena
hakemistoon `~/bin`. Muunkin asennushakemiston voi valita seuraavalla
tavalla: `make install bindir=/jokin/muu/hakemisto`.

[SBCL]:     http://www.sbcl.org/
[Readline]: http://www.gnu.org/software/readline/
[SQLite3]:  http://www.sqlite.org/
[Debian]:   http://www.debian.org/
[QL]:       http://www.quicklisp.org/


Käyttö
------

Kun ohjelman `koas` käynnistää ensimmäisen kerran, luodaan
tietokantatiedosto `~/.config/koas.db`. Siihen tallentuvat kaikki
käyttäjän syöttämät tiedot. Tiedostosta kannattaa pitää hyvää huolta,
eli se kannattaa varmuuskopioida riittävän usein.

Ohjelman käyttö on yleensä vuorovaikutteista ja perustuu komentojen
syöttämiseen näppäimistöltä. Kun ohjelma on käynnissä, komentojen kehote
on `KOAS>`, jolloin komentoja voi syöttää. Ohjeita komentoihin ja niiden
käyttöön saa komennoilla `?`, `??` ja `???`.

Useimmat toiminnot ovat käytettävissä myös ei-vuorovaikutteisesti eli
komentotulkista. Esimerkiksi kaikki tiedonhakukomennot ovat tällaisia.
Kirjoittamalla ohjelman komennot suoraan komentoriviargumenteiksi
suoritetaan vain kyseinen toiminto eikä käynnistetä vuorovaikutteista
tilaa lainkaan.


Tekijä ja tekijänoikeus
-----------------------

Ohjelman tekijä on Teemu Likonen <<tlikonen@iki.fi>> (PGP:
[4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]), ja ohjelmaa
levitetään [The GNU General Public License][GPL] -nimisellä lisenssillä.
Lisenssin teksti on linkin takana englanninkielisenä. Lisenssin
ajatuksena on, että ohjelmaa saa käyttää ja levittää vapaasti. Ohjelmaan
saa myös vapaasti tehdä omia muutoksia, mutta jos levittää omaa
muunneltua versiota, täytyy ohjelman muunneltu lähdekoodi antaa muiden
käyttöön. Muunnellut versiot täytyy jakaa tällä samalla
ohjelmistolisenssillä (tai sen uudemmalla versiolla).

Ohjelmaa levitetään siinä toivossa, että siitä on hyötyä muille.
Ohjelman tekijä ei kuitenkaan anna mitään takuita ohjelmalle, ei edes
lupausta, että se sopii johonkin tiettyyn käyttötarkoitukseen.

[GPL]: http://www.gnu.org/licenses/gpl-3.0.html
[PGP]: http://koti.kapsi.fi/~dtw/pgp-key.asc


Copyright and Licence
---------------------

Copyright (C) 2013-2015 Teemu Likonen <<tlikonen@iki.fi>>

PGP: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>
