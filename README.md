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
  * opetusryhmän oppilaiden arvosanat koottuna.

Ohjelma myös laskee suoritusten keskiarvot automaattisesti ja hyödyntää
laskemisessa käyttäjän määrittämiä painokertoimia. Ohjelmassa on
tekstipohjainen käyttöliittymä ja myös tulosteet ovat tekstimuotoisia
taulukoita.


Asentaminen ja käyttö
---------------------

_Koas_ toimii ainakin GNU/Linux-käyttöjärjestelmissä. Se on ohjelmoitu
[Rust][]-kielellä, ja sen kääntäminen lähdekoodista vaatii Rust-kielen
kehitysympäristön, jonka voi asentaa ohjelmointikielen verkkosivun
ohjeiden avulla.

Ohjelman voi kääntää lähdekoodista ja käännetyn ohjelman asentaa
komennolla `cargo install --path .` Kääntämisen yhteydessä ladataan
automaattisesti useita ohjelmointikirjastoja, joita Koas tarvitsee.

Arvosanatietokantaa varten Koas tarvitsee erillisen
[PostgreSQL][Psql]-tietokantapalvelimen. Se täytyy asentaa erikseen.
Koas-ohjelma sisältää ohjeita tietokannan valmisteluun Koasia varten
(ks. [help/database.txt](help/database.txt)).

Koas käynnistetään komennolla `koas`. Komento käynnistää ohjelman
vuorovaikutteiseen tilaan, jossa kehote `koas>` ilmaisee, että komentoja
voi syöttää. Komennolla `?` tulostuvat ohjeet ohjelman komennoista.
Ohjelman komentorivillä valitsin `-h` tulostaa myös apua. Ohjelman
ohjetekstit ovat alihakemistossa [help](help).

[Rust]:     https://www.rust-lang.org/
[Psql]:     https://www.postgresql.org/


Tekijä ja tekijänoikeus
-----------------------

Tekijä: Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP-avain: [6965F03973F0D4CA22B9410F0F2CAE0E07608462][PGP]

Ohjelmaa levitetään [The GNU General Public License][GPL] -nimisellä
lisenssillä. Lisenssin teksti on linkin takana englanninkielisenä.
Lisenssin ajatuksena on, että ohjelmaa saa käyttää ja levittää vapaasti.
Ohjelmaan saa myös vapaasti tehdä omia muutoksia, mutta jos levittää
omaa muunneltua versiota, täytyy ohjelman muunneltu lähdekoodi antaa
muiden käyttöön. Muunnellut versiot täytyy jakaa tällä samalla
ohjelmistolisenssillä (tai sen uudemmalla versiolla).

Ohjelmaa levitetään siinä toivossa, että siitä on hyötyä muille.
Ohjelman tekijä ei kuitenkaan anna mitään takuita ohjelmalle, ei edes
lupausta, että se sopii johonkin tiettyyn käyttötarkoitukseen.

[GPL]: http://www.gnu.org/licenses/gpl-3.0.html
[PGP]: http://www.iki.fi/tlikonen/pgp-key.asc


Copyright and Licence
---------------------

Author: Teemu Likonen <<tlikonen@iki.fi>>

OpenPGP key: [6965F03973F0D4CA22B9410F0F2CAE0E07608462][PGP]

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>
