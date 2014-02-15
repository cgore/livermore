;;;; Copyright (c) 2005 -- 2014, Christopher Mark Gore,
;;;; Soli Deo Gloria,
;;;; All rights reserved.
;;;;
;;;; 2317 South River Road, Saint Charles, Missouri 63303 USA.
;;;; Web: http://cgore.com
;;;; Email: cgore@cgore.com
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;;
;;;;     * Redistributions of source code must retain the above copyright
;;;;       notice, this list of conditions and the following disclaimer.
;;;;
;;;;     * Redistributions in binary form must reproduce the above copyright
;;;;       notice, this list of conditions and the following disclaimer in the
;;;;       documentation and/or other materials provided with the distribution.
;;;;
;;;;     * Neither the name of Christopher Mark Gore nor the names of other
;;;;       contributors may be used to endorse or promote products derived from
;;;;       this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(in-package "STOCKS")

(defparameter *dow-jones-indices*
  '(("^dja" "Dow Jones 65 Composite Average")
    ("^dji" "Dow Jones 30 Industrial Average")
    ("^djt" "Dow Jones 20 Transportation Average")
    ("^dju" "Dow Jones 15 Utilities Average")))

(defparameter *nyse-indices*
  '(("^nya" "New York Stock Exchange Composite")
    ("^nin" "New York Stock Exchange International 100")
    ("^ntm" "New York Stock Exchange TMT")
    ("^nus" "New York Stock Exchange U.S. 100")
    ("^nwl" "New York Stock Exchange World Leaders")))

(defparameter *nasdaq-indices*
  '(("^ixbk" "NASDAQ Banks")
    ("^nbi"  "NASDAQ Biotech")
    ("^ixic" "NASDAQ NAS/NMS Composite")
    ("^ixk"  "NASDAQ Computers")
    ("^ixf"  "NASDAQ Financials 100")
    ("^ixid" "NASDAQ Industrials")
    ("^ixis" "NASDAQ Insurance")
    ("^ixq"  "NASDAQ National Market Composite")
    ("^ixfn" "NASDAQ Financials")
    ("^ixut" "NASDAQ Telecommunications")
    ("^ixtr" "NASDAQ Transportation")
    ("^ndx"  "NASDAQ 100")))

(defparameter *standard-and-poors-indices*
  '(("^oex"    "Standard and Poor's 100 Index")
    ("^mid"    "Standard and Poor's 400 Mid Cap")
    ("^gspc"   "Standard and Poor's 500 Index")
    ("^spsupx" "Standard and Poor's 1500 Super Comp")
    ("^sml"    "Standard and Poor's 600 Small Cap")))

(defparameter *other-us-indices* 
  '(("^xax"  "AMEX Composite")
    ("^iix"  "AMEX Internet")
    ("^nwx"  "AMEX Networking")
    ("^dwc"  "Wilshire 5000 TOT")
    ("^xmi"  "Major Market")
    ("^pse"  "Pacific Exchange Technology")
    ("^soxx" "Philadelphia Semiconductor")
    ("^dot"  "TSC Internet")
    ("^rui"  "Russell 1000")
    ("^rut"  "Russell 2000")
    ("^rua"  "Russell 3000")))

(defparameter *us-treasury-indices*
  '(("^tnx" "U.S. Treasury 10-Year Note")
    ("^irx" "U.S. Treasury 13-Week Bill")
    ("^tyx" "U.S. Treasury 30-Year Bond")
    ("^fvx" "U.S. Treasury 5-Year Note")))


(defparameter *us-commodities-indices*
  '(("^djc"  "Dow Jones AIG Commodity")
    ("^djs2" "Dow Jones Industrial Average Settle")
    ("^xau"  "Philadelphia Gold and Silver")))

(defparameter *misc-stocks*
  '(("aa"    "Alcoa, Inc.")
    ("aapl"  "Apple Computer, Inc.")
    ("aar"   "AMR Pines Corp.")
    ("adsx"  "Applied Digital")
    ("aig"   "American International Group")
    ("altr"  "Altera Corp.")
    ("amat"  "Applied Materials, Inc.")
    ("amgn"  "Amgen")
    ("axp"   "American Express Company")
    ("ba"    "Boeing Company")
    ("brcm"  "Broadcom Corp.")
    ("bud"   "Anheuser-Busch Companies Inc.")
    ("c"     "CitiGroup")
    ("cat"   "Caterpillar, Inc.")
    ("ccur"  "Concurrent Comp")
    ("cien"  "Ciena Corp.")
    ("cpn"   "Calpine Corp.")
    ("csco"  "Cisco Systems, Inc.")
    ("dd"    "Du Pont & Company")
    ("dell"  "Dell Computer")
    ("dgx"   "Quest Diagnostics, Inc.")
    ("dis"   "Walt Disney Co.")
    ("dph"   "Delphi Corp.")
    ("dyn"   "Dynergy Inc.")
    ("emc"   "EMC Corp.")
    ("ep"    "El Paso Corp.")
    ("ericy" "LM Ericsson Telephone Co.")
    ("flex"  "Flextronics")
    ("ge"    "General Electric Co.")
    ("gis"   "General Mills")
    ("gm"    "General Motors")
    ("gmst"  "Gemstar-TV Guide International Inc.")
    ("hd"    "Home Depot, Inc.")
    ("hon"   "Honeywell International")
    ("hpq"   "Hewlett-Packard")
    ("ibm"   "International Business Machines")
    ("intc"  "Intel Corp.")
    ("jdsu"  "JDS Uniphase Corp.")
    ("jnj"   "Johnson and Johnson")
    ("jpm"   "J. P. Morgan Chase")
    ("klac"  "KLA Tencor")
    ("ko"    "Coca-Cola Company")
    ("l"     "Liberty Media")
    ("lu"    "Lucent Tech.")
    ("mcd"   "McDonald's Corp.")
    ("mer"   "Merrill Lynch")
    ("mmm"   "3M Company")
    ("mo"    "Altria Group")
    ("mot"   "Motorola Inc.")
    ("mrk"   "Merck and Company, Inc.")
    ("msft"  "Microsoft Corp.")
    ("mwd"   "Morgan Stanley Dean Witter")
    ("nok"   "Nokia Corp.")
    ("nvls"  "Novellus Systems")
    ("nxtl"  "Nextel Communications")
    ("nt"    "Nortel Networks Corp.")
    ("ntap"  "Network Appliance")
    ("orcl"  "Oracle Corp.")
    ("pfe"   "Pfizer Inc.")
    ("pg"    "Procter and Gamble")
    ("psft"  "Peoplesoft Inc.")
    ("qcom"  "Qualcomm Inc.")
    ("qlgc"  "QLogic Corp.")
    ("rhat"  "Red Hat")
    ("sbc"   "SBC Communications")
    ("sebl"  "Siebel Systems")
    ("siri"  "Sirius Satellite Radio, Inc.")
    ("sunw"  "Sun Microsystems, Inc.")
    ("t"     "AT&T Corp")
    ("txn"   "Texas Instruments")
    ("tyc"   "Tyco International")
    ("utx"   "United Tech Corp.")
    ("vrsn"  "Verisign Inc.")
    ("vz"    "Verizon Communications")
    ("wmb"   "Williams Companies, Inc.")
    ("wmt"   "Wal-Mart Stores, Inc.")
    ("xom"   "Exxon Mobil")
    ("yhoo"  "Yahoo Inc.")))

;;; These are the stocks that make up the Dow Jones Industrial Average.
(defparameter *^DJI-components*
  '("aa" "aig" "axp" "ba" "c" "cat" "dd" "dis" "ge" "gm"
    "hd" "hon" "hpq" "ibm" "intc" "jnj" "jpm" "ko" "mcd" "mmm"
    "mo" "mrk" "msft" "pfe" "pg" "sbc" "utx" "vz" "wmt" "xom"))

;;; This is a far-from-complete listing of stock ticker symbols and long names.
(defparameter *ticker-descriptions* (make-hash-table :test #'equal))
(mapc #'(lambda (entry)
          (setf (gethash (string-downcase (first entry))
                         *ticker-descriptions*)
                (second entry)))
       (append *dow-jones-indices*
               *nyse-indices*
               *nasdaq-indices*
               *standard-and-poors-indices*
               *other-us-indices*
               *us-treasury-indices*
               *us-commodities-indices*
               *misc-stocks*))

(defgeneric stock-description (item))

(defmethod stock-description ((string string))
  (gethash (string-downcase string) *ticker-descriptions*))

(defmethod stock-description ((symbol symbol))
  (stock-description (symbol-name symbol)))
