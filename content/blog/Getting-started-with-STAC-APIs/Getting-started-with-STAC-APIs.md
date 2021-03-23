---
title: Getting started with STAC APIs
date: 2021-03-23T21:26:42+00:00
description: An introduction to a small console application called `stac-repl` for browsing STAC APIs.
---

[STAC] is a specification for "enabling online search and discovery of geospatial assets."
Right now, a lot of work has gone into the core specification, with a 1.0.0-rc1 for the
core specification released 20 days ago. Following the core spec's 1.0.0 release, more
work will go into the API spec.

<center><img src="https://stacspec.org/images/logo/STAC-04.png" width=200/></center>

The tooling story on the way to standing up a STAC API is pretty well ironed out. If you want to create
a catalog that follows all the rules in the pretty sizable
[best practices doc], use [ `PySTAC` ]. If you just want a typed STAC datamodel for building
applications on top of STAC, use [ `stac-pydantic` ] or [ `stac4s` ]. They're both dogfooded
in STAC API implementations, `stac-pydantic` in [ `arturo-stac-api` ] and `stac4s` in [ `Franklin` ].

One area where tools are lacking is clients. Most clients in [stacindex.org] are either libraries
that require you to write code or full on frontend applications. An exception to this
is [ `sat-search` ] , which is a CLI tool.
To support a more interactive browsing adventure, I wrote [ `stac-repl` ], which is built on top
of a PureScript client I wrote called [ `purescript-stac` ].
I completed both of these as part of 10% time work at [Azavea].

The goal of `stac-repl` is to provide an interactive, ipython-ish experience around browsing
STAC APIs. You can run it with `node` by downloading the bundled js file from the [v0.1.1 release] or
in a container with `docker run jisantuc/stac-repl:0.1.1` .

You should be able to figure out everything you can do by mashing the `<TAB>` key. For example, 
when you start up `stac-repl` , you only get one completion:

``` 

stac > <TAB>
stac > set root url
```

For this example, I'll point the root to a catalog of [AVIRIS] data that can be found
at `https://franklin.nasa-hsi.azavea.com` . Tabbing from there, 
commands get a little more interesting:

``` 

stac > set root url https://franklin.nasa-hsi.azavea.com
stac https://franklin.nasa-hsi.azavea.com > <TAB>
set root url      get conformance   set collection    list collections
```

Now that we know where the catalog root is, we can check its conformance information, list collections, 
or pick a specific collection. We don't know any collections yet, so let's list some (naturally, by typing
just an `li` and a `<TAB>` ):

``` 

stac https://franklin.nasa-hsi.azavea.com > list collections
aviris-classic: AVIRIS is an acronym for the Airborne Visible InfraRed Imaging Spectrometer. AVIRIS is a premier instrument in the realm of Earth Remote Sensing. It is a unique optical sensor that delivers calibrated images of the upwelling spectral radiance in 224 contiguous spectral channels (also called bands) with wavelengths from 400 to 2500 nanometers (nm). AVIRIS has been flown on four aircraft platforms: NASA's ER-2 jet, Twin Otter International's turboprop, Scaled Composites' Proteus, and NASA's WB-57. The ER-2 flies at approximately 20 km above sea level, at about 730 km/hr. The Twin Otter aircraft flies at 4km above ground level at 130km/hr. AVIRIS has flown all across the US, plus Canada and Europe. This catalog contains all AVIRIS missions from 2006 - 2019.

aviris-ng: AVIRIS is an acronym for the Airborne Visible InfraRed Imaging Spectrometer. AVIRIS is a premier instrument in the realm of Earth Remote Sensing. It is a unique optical sensor that delivers calibrated images of the upwelling spectral radiance in 224 contiguous spectral channels (also called bands) with wavelengths from 400 to 2500 nanometers (nm). AVIRIS has been flown on four aircraft platforms: NASA's ER-2 jet, Twin Otter International's turboprop, Scaled Composites' Proteus, and NASA's WB-57. The ER-2 flies at approximately 20 km above sea level, at about 730 km/hr. The Twin Otter aircraft flies at 4km above ground level at 130km/hr. AVIRIS has flown all across the US, plus Canada and Europe. This catalog contains all AVIRIS missions from 2006 - 2019.

aviris-l2-cogs: AVIRIS L2 Refl Imagery converted to pixel-interleaved COGs

aviris-l2-chips: A STAC Collection containing COGs created by the nasa-hsi cog-clip activator
```

When you list collections, `stac-repl` remembers their ids, so when you want
to pick a collection you can also get tab completions. For example, since all of these collection IDs start with
`aviris` , `set collection a<TAB><TAB>` completes to:

``` 

stac https://franklin.nasa-hsi.azavea.com > set collection aviris-
set collection aviris-classic   set collection aviris-l2-chips  set collection aviris-l2-cogs   set collection aviris-ng
```

I'll pick `aviris-ng` .

``` 

stac https://franklin.nasa-hsi.azavea.com > set collection aviris-ng
Set context to collection aviris-ng
```

Now that I'm in a different context, the available commands are different:

``` 

stac https://franklin.nasa-hsi.azavea.com > <TAB>
view               unset collection   locate collection  get conformance    list items         next page          locate item
```

Here, the new commands are `view` , `unset collection` , `list items` , `next page` , and `locate item` . Let's see what items are available:

``` 

stac https://franklin.nasa-hsi.azavea.com > list items
...
Id: "aviris_ang20140605t182232"
Extensions: []
Bbox: [-122.73583300000001,38.558091,-122.676149,38.632746999999995]
Assets:
ftp: ftp://avng_dp:P73axIvP@avng.jpl.nasa.gov/y14_data/ang20140605t182232.tar.gz ((VendorMediaType "application/gzip"))
rgb: http://avirisng.jpl.nasa.gov/aviris_locator/y14_RGB/ang20140605t182232_RGB.jpeg (JPEG)
kml_overlay: http://avirisng.jpl.nasa.gov/aviris_locator/y14_KML/ang20140605t182232_overlay_KML.kml ((VendorMediaType "application/vnd.google-earth.kml+xml"))
rgb_small: http://avirisng.jpl.nasa.gov/aviris_locator/y14_RGB/ang20140605t182232_RGB-W200.jpg (JPEG)
flight_log: http://avirisng.jpl.nasa.gov/cgi/flights_14.cgi?step=view_flightlog&flight_id=ang20140605t (HTML)
kml_outline: http://avirisng.jpl.nasa.gov/aviris_locator/y14_KML/ang20140605t182232_outline_KML.kml ((VendorMediaType "application/vnd.google-earth.kml+xml"))
```

The API returns them 30 at a time, but I'm only showing the last one on the first page to avoid a huge wall of text.
Similarly to listing collections, `stac-repl` remembers item IDs so that we can tab complete them for item-specific
commands.

In this case, the only item-specific command is `locate item` , so tabbing after that, we can eventually get to

``` 

stac https://franklin.nasa-hsi.azavea.com > locate item aviris_ang20140605t182232
```

This gets us a delightful [Mapscii] map.

![](./mapscii-map.png)

This first release of `stac-repl` covers only the most basic interactions, but I'm looking forward to building
out support for more features like [search] and [transactions].

[ `PySTAC` ]: https://github.com/stac-utils/pystac
[ `stac-repl` ]: https://github.com/jisantuc/stac-repl
[best practices doc]: https://github.com/radiantearth/stac-spec/blob/master/best-practices.md
[ `arturo-stac-api` ]: https://github.com/stac-utils/arturo-stac-api
[ `Franklin` ]: https://azavea.github.io/franklin/
[ `purescript-stac` ]: https://github.com/jisantuc/purescript-stac
[v0.1.1 release]: https://github.com/jisantuc/stac-repl/releases/tag/v0.1.1
[AVIRIS]: https://aviris.jpl.nasa.gov/
[Mapscii]: https://github.com/rastapasta/mapscii/
[search]: https://github.com/radiantearth/stac-api-spec/tree/v1.0.0-beta.1/item-search
[transactions]: https://github.com/radiantearth/stac-api-spec/tree/v1.0.0-beta.1/ogcapi-features/extensions/transaction
[stacindex.org]: https://stacindex.org/ecosystem?category=Client
[ `sat-search` ]: https://github.com/sat-utils/sat-search
[Azavea]: https://www.azavea.com/
[STAC]: https://stacspec.org/
