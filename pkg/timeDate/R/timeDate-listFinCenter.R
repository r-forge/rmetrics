
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 DESCRIPTION:
#  listFinCenter             Lists all supported financial centers
################################################################################

listFinCenter <-
    function(pattern = ".*")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Lists available Financial Centers

    # Arguments:
    #   pattern - a pattern character string which can be recognized
    #       by the 'grep' functs. Wild cards are allowed.

    # Value:
    #   Returns a printed list of financial centers.

    # Example:
    #   > listFinCenter("Europe/*")
    #    [1] "Europe/Amsterdam"   "Europe/Andorra"     "Europe/Athens"
    #    [4] "Europe/Belfast"     "Europe/Belgrade"    "Europe/Berlin"
    #    [7] "Europe/Bratislava"  "Europe/Brussels"    "Europe/Bucharest"
    #   [10] "Europe/Budapest"    "Europe/Chisinau"    "Europe/Copenhagen"
    #   [13] "Europe/Dublin"      "Europe/Gibraltar"   "Europe/Helsinki"
    #   [16] "Europe/Istanbul"    ...

    # FUNCTION:

    ## GNB: now the list is generated when the DST rules are generated,
    ##      see the end of timeDate-DaylightSavingTime.R
    ##
    ##      FinCenterList => .FinCenterList  (note the leading dot)

    ##  "Pacific/Easter" is excluded as it is in conflict with the feast Easter.
    ## TODO: include it as "Easter_Island"?

    # Financial Centers:
    if (pattern == "*") pattern = "\\\\*"

    # Return Value:
    sort(as.character(.FinCenterList[grep(pattern = pattern,
        x = .FinCenterList)]))
}


################################################################################
