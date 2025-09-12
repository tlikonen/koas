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

Tavallisimpia komentoja lähdekoodihakemistossa:

  * `cargo build` ohjelman kääntäminen lähdekoodista
  * `cargo run` ohjelman suorittaminen lähdekoodihakemistossa
  * `cargo install --path .` ohjelman asentaminen polkuun `~/.cargo/bin/`
  * `cargo clean` kääntämistiedostojen poisto

Yksinkertaisimmin ohjelman voi kääntää ja asentaa komennolla `cargo
install --path .` Kääntämisen yhteydessä ladataan automaattisesti useita
ohjelmointikirjastoja, joita Koas tarvitsee.

Arvosanatietokantaa varten Koas tarvitsee erillisen
[PostgreSQL][Psql]-tietokantapalvelimen. Se täytyy asentaa erikseen tai
olla käytettävissä jossakin verkkopalvelussa. Koas-ohjelma sisältää
ohjeita tietokannan valmisteluun Koasia varten (ks.
[help/database.txt](help/database.txt)). Muutkin ohjelman ohjetekstit
ovat luettavissa alihakemistossa [help](help).

Koas käynnistetään komennolla `koas`. Komento käynnistää ohjelman
vuorovaikutteiseen tilaan, jossa kehote `koas>` ilmaisee, että komentoja
voi syöttää. Komennolla `?` tulostuvat ohjeet ohjelman komennoista. Jos
ohjelman käynnistää käyttämällä valitsinta `-h` (`koas -h`), tulostuu
tietoa ohjelman komentoriviargumenteista.

[Rust]:     https://www.rust-lang.org/
[Psql]:     https://www.postgresql.org/


Tekijä ja tekijänoikeus
-----------------------

Tekijä: Teemu Likonen <<tlikonen@iki.fi>> ([kotisivu][koti], [PGP][])

Ohjelmaa levitetään [The GNU General Public License][GPL] -nimisellä
lisenssillä. Lisenssin teksti on linkin takana englanninkielisenä.
Lisenssin ajatuksena on, että ohjelmaa saa käyttää ja levittää vapaasti.
Ohjelmaan saa myös tehdä omia muutoksia, mutta jos levittää omaa
muunneltua versiota, täytyy ohjelman muunneltu lähdekoodi antaa muiden
käyttöön. Muunnellut versiot täytyy jakaa tällä samalla
ohjelmistolisenssillä (tai sen uudemmalla versiolla).

Ohjelmaa levitetään siinä toivossa, että siitä on hyötyä muille.
Ohjelman tekijä ei kuitenkaan anna mitään takuita ohjelmalle, ei edes
lupausta, että se sopii johonkin tiettyyn käyttötarkoitukseen.

[koti]: http://www.iki.fi/tlikonen/
[PGP]:  http://www.iki.fi/tlikonen/teemu.pgp
[GPL]:  http://www.gnu.org/licenses/gpl-3.0.html


Copyright and Licence
---------------------

Author: Teemu Likonen <<tlikonen@iki.fi>> ([web][koti], [PGP][])

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

The license text: <http://www.gnu.org/licenses/gpl-3.0.html>
