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
laskemisessa käyttäjän määrittämiä painokertoimia. Ohjelmassa on
tekstipohjainen käyttöliittymä ja myös tulosteet ovat tekstimuotoisia
taulukoita.

Tiedot tallennetaan joko SQLite-tietokantatiedostoon tai erilliselle
PostgreSQL-tietokantapalvelimelle.


Asentaminen
-----------

_Kouluarvosanatietokanta_ toimii ainakin GNU/Linux-käyttöjärjestelmissä.
Ohjelman kääntäminen ja suorittaminen vaatii [SBCL][]-nimisen Common
Lisp -toteutuksen. Lisäksi asennettuna täytyy olla kehittäjäversio
[Readline][]- ja [SQLite3][]-kirjastosta. Esimerkiksi [Debian
GNU/Linux][Debian] -käyttöjärjestelmässä edellä mainitut ovat
asennuspaketeissa nimeltä `sbcl`, `libreadline6-dev` ja
`libsqlite3-dev`.

Ensimmäisellä kerralla kääntämisen yhteydessä käytetään `wget`-ohjelmaa,
jolla haetaan internetistä [Quicklisp][QL]. Quicklispin avulla ladataan
internetistä automaattisesti eräitä Common Lisp -kirjastoja.

Ohjelma käännetään komennolla `make` ja asennetaan komennolla `make
install`. Oletuksena ohjelma asennetaan hakemistoon `/usr/local/bin`.
Asennushakemiston voi määrittää makefile-muuttujalla `bindir`.
Käytettävän [SBCL][]:n polun voi määrittää muuttujalla `sbcl`.
Esimerkki: `make sbcl=... bindir=...`. Käytetyt muuttujat tallentuvat
tiedostoon `config.mk`.


[SBCL]:     http://www.sbcl.org/
[Readline]: http://www.gnu.org/software/readline/
[SQLite3]:  http://www.sqlite.org/
[Debian]:   http://www.debian.org/
[QL]:       http://www.quicklisp.org/


Käyttö
------

Koas käynnistetään yleensä komennolla `koas`. Se käynnistää ohjelman
vuorovaikutteiseen tilaan, jossa kehote `Koas>` ilmaisee, että komentoja
voi syöttää. Komennolla `?` tulostuu ohjeet kaikista ohjelman
komennoista.

Ensimmäisellä käynnistyskerralla luodaan automaattisesti
SQLite-tietokantatiedosto `~/.config/koas.db`, johon ohjelman asetukset
ja tietokanta oletuksena tallentuu.

Ohjelmassa on myös toimintoja ja asetuksia, joita käytetään vain
komentoriviargumenttien kautta. Niistä saa lisätietoa, kun ohjelman
käynnistää antamalla argumentin `-h` tai `--ohje`.


Tekijä ja tekijänoikeus
-----------------------

Ohjelman tekijä on Teemu Likonen <<tlikonen@iki.fi>> (OpenPGP-avain:
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
[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc


Copyright and Licence
---------------------

Copyright (C) 2013-2021 Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP key: [4E10 55DC 84E9 DFF6 13D7 8557 719D 69D3 2453 9450][PGP]

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>
