timeNthNdayInMonth
function (charvec, nday = 1, nth = 1, format = "%Y-%m-%d", zone = "",
FinCenter = "")
{
if (zone == "")
zone <- getRmetricsOptions("myFinCenter")
if (FinCenter == "")
FinCenter <- getRmetricsOptions("myFinCenter")
lt = strptime(charvec, format, tz = "GMT")
lt1 = lt
lt1$mday = 1
ct = 24 * 3600 * (as.integer(julian.POSIXt(lt1)) + (nth -
1) * 7 + (nday - lt1$wday)%%7)
class(ct) = "POSIXct"
timeDate(format(ct), format = format, zone = zone, FinCenter = FinCenter)
}
