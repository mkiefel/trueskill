#!/bin/bash

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2007.csv \
            data/bundesliga/2015.08.25_with_odds/data_2008.csv \
            data/bundesliga/2015.08.25_with_odds/data_2009.csv \
            data/bundesliga/2015.08.25_with_odds/data_2010.csv \
            data/bundesliga/2015.08.25_with_odds/data_2011.csv \
            data/bundesliga/2015.08.25_with_odds/data_2012.csv \
            data/bundesliga/2015.08.25_with_odds/data_2013.csv \
       ) \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2014.csv \
       ) \
      data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2014.csv
mv player.csv player_2014.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2006.csv \
            data/bundesliga/2015.08.25_with_odds/data_2007.csv \
            data/bundesliga/2015.08.25_with_odds/data_2008.csv \
            data/bundesliga/2015.08.25_with_odds/data_2009.csv \
            data/bundesliga/2015.08.25_with_odds/data_2010.csv \
            data/bundesliga/2015.08.25_with_odds/data_2011.csv \
            data/bundesliga/2015.08.25_with_odds/data_2012.csv \
       ) \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2013.csv \
       ) \
      data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2013.csv
mv player.csv player_2013.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2005.csv \
            data/bundesliga/2015.08.25_with_odds/data_2006.csv \
            data/bundesliga/2015.08.25_with_odds/data_2007.csv \
            data/bundesliga/2015.08.25_with_odds/data_2008.csv \
            data/bundesliga/2015.08.25_with_odds/data_2009.csv \
            data/bundesliga/2015.08.25_with_odds/data_2010.csv \
            data/bundesliga/2015.08.25_with_odds/data_2011.csv \
       ) \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2012.csv \
       ) \
      data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2012.csv
mv player.csv player_2012.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2004.csv \
            data/bundesliga/2015.08.25_with_odds/data_2005.csv \
            data/bundesliga/2015.08.25_with_odds/data_2006.csv \
            data/bundesliga/2015.08.25_with_odds/data_2007.csv \
            data/bundesliga/2015.08.25_with_odds/data_2008.csv \
            data/bundesliga/2015.08.25_with_odds/data_2009.csv \
            data/bundesliga/2015.08.25_with_odds/data_2010.csv \
       ) \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2011.csv \
       ) \
      data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2011.csv
mv player.csv player_2011.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2003.csv \
            data/bundesliga/2015.08.25_with_odds/data_2004.csv \
            data/bundesliga/2015.08.25_with_odds/data_2005.csv \
            data/bundesliga/2015.08.25_with_odds/data_2006.csv \
            data/bundesliga/2015.08.25_with_odds/data_2007.csv \
            data/bundesliga/2015.08.25_with_odds/data_2008.csv \
            data/bundesliga/2015.08.25_with_odds/data_2009.csv \
       ) \
      <(cat \
            data/bundesliga/2015.08.25_with_odds/header.csv \
            data/bundesliga/2015.08.25_with_odds/data_2010.csv \
       ) \
      data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2010.csv
mv player.csv player_2010.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
    <(cat \
          data/bundesliga/2015.08.25_with_odds/header.csv \
          data/bundesliga/2015.08.25_with_odds/data_2002.csv \
          data/bundesliga/2015.08.25_with_odds/data_2003.csv \
          data/bundesliga/2015.08.25_with_odds/data_2004.csv \
          data/bundesliga/2015.08.25_with_odds/data_2005.csv \
          data/bundesliga/2015.08.25_with_odds/data_2006.csv \
          data/bundesliga/2015.08.25_with_odds/data_2007.csv \
          data/bundesliga/2015.08.25_with_odds/data_2008.csv \
     ) \
    <(cat \
          data/bundesliga/2015.08.25_with_odds/header.csv \
          data/bundesliga/2015.08.25_with_odds/data_2009.csv \
     ) \
    data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2009.csv
mv player.csv player_2009.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
    <(cat \
          data/bundesliga/2015.08.25_with_odds/header.csv \
          data/bundesliga/2015.08.25_with_odds/data_2001.csv \
          data/bundesliga/2015.08.25_with_odds/data_2002.csv \
          data/bundesliga/2015.08.25_with_odds/data_2003.csv \
          data/bundesliga/2015.08.25_with_odds/data_2004.csv \
          data/bundesliga/2015.08.25_with_odds/data_2005.csv \
          data/bundesliga/2015.08.25_with_odds/data_2006.csv \
          data/bundesliga/2015.08.25_with_odds/data_2007.csv \
     ) \
    <(cat \
          data/bundesliga/2015.08.25_with_odds/header.csv \
          data/bundesliga/2015.08.25_with_odds/data_2008.csv \
     ) \
    data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2008.csv
mv player.csv player_2008.csv

env LD_LIBRARY_PATH=lbfgs/Lbfgsb.3.0/ ./dist/build/rolling_predict/rolling_predict \
    <(cat \
          data/bundesliga/2015.08.25_with_odds/header.csv \
          data/bundesliga/2015.08.25_with_odds/data_2000.csv \
          data/bundesliga/2015.08.25_with_odds/data_2001.csv \
          data/bundesliga/2015.08.25_with_odds/data_2002.csv \
          data/bundesliga/2015.08.25_with_odds/data_2003.csv \
          data/bundesliga/2015.08.25_with_odds/data_2004.csv \
          data/bundesliga/2015.08.25_with_odds/data_2005.csv \
          data/bundesliga/2015.08.25_with_odds/data_2006.csv \
     ) \
    <(cat \
          data/bundesliga/2015.08.25_with_odds/header.csv \
          data/bundesliga/2015.08.25_with_odds/data_2007.csv \
     ) \
    data/bundesliga/2015.08.25_with_odds/knobs.json
mv games.csv games_2007.csv
mv player.csv player_2007.csv
