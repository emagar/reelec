tmp$cycle <- tmp$yr
tmp$cycle[tmp$edon==1] <- mapvalues(tmp$cycle[tmp$edon==1],
                                             from = c(1980,1983,1986,1989,1992,1995,1998,2001,2004,2007,2010,2013,2016,2019,2021,2024),
                                             to = 4:19)
tmp$cycle[tmp$edon==2] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==3] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2011, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==4] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==5] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2009, 2013, 2017, 2018, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==6] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==7] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1995, 1998, 2001, 2004, 2007, 2010, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==8] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==9] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 11:19)
tmp$cycle[tmp$edon==10] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2019, 2022, 2025)
                                             to = 4:19)
tmp$cycle[tmp$edon==11] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==12] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1993, 1996, 1999, 2002, 2005, 2008, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==13] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2016, 2020, 2024)
                                                 to = 4:17)
tmp$cycle[tmp$edon==14] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1992, 1995, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==15] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1981, 1984, 1987, 1990, 1993, 1996, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==16] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2011, 2015, 2018, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==17] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==18] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==19] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==20] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==21] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2018, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==22] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==23] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1981, 1984, 1987, 1990, 1993, 1996, 1999, 2002, 2005, 2008, 2010, 2013, 2016, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==24] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==25] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==26] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==27] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2003, 2006, 2009, 2012, 2015, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==28] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==29] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==30] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1991, 1994, 1997, 2000, 2004, 2007, 2010, 2013, 2017, 2021, 2024)
                                             to = 4:18)
tmp$cycle[tmp$edon==31] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1981, 1984, 1987, 1990, 1993, 1995, 1998, 2001, 2004, 2007, 2010, 2012, 2016, 2018, 2021, 2024)
                                             to = 4:19)
tmp$cycle[tmp$edon==32] <- mapvalues(tmp$cycle[tmp$edon==],
                                             from = c(1979, 1982, 1985, 1988, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013, 2016, 2018, 2021, 2024)
                                             to = 4:19)

