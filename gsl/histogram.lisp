;;;; gsl/histogram.lisp
;;;;
;;;; Histograms provide a convenient way of summarizing the
;;;; distribution of a set of data. A histogram consists of a set of
;;;; bins which count the number of events falling into a given range
;;;; of a continuous variable x.  Once a histogram has veen created it
;;;; can also be converted into a probability distribution
;;;; function. The library provides efficient routines for selecting
;;;; random samples from probability distributions. This can be useful
;;;; for generating simulations based on real data.

;;;; Copyright (C) 2017 Takahiro Ishikawa
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see http://www.gnu.org/licenses/.

(in-package "GSL")


;;; function


;;; copying histograms

(defun histogram-memcpy (dest src)
  "This function copies the histogram src into the pre-existing
histogram dest, making dest into an exact copy of src. The two
histograms must be of the same size."
  (gsl_histogram_memcpy (pointer dest) (pointer src))
  dest)

(defun histgram-clone (src)
  "This function returns a pointer to a newly created histogram which
is an exact copy of the histogram src."
  (make-instance 'histogram :pointer (gsl_histogram_clone (pointer src))))


;;; updating and accessing histogram elements

(defun histogram-increment (h x)
  "This function updates the histogram h by adding one (1.0) to the
bin whose range contains the coordinate x.
If x lies in the valid range of the histogram then the function
returns zero to indicate success. If x is less than the lower limit of
the histogram then none of bins are modified. Similary, if the value
of x is greater than or equal to the upper limit of the histogram then
none of bins are modified."
  (gsl_histogram_increment (pointer h) (coerce x 'double-float))
  h)

(defun histogram-accumulate (h x weight)
  "This function is similar to histogram-increment but increases the
value of the appropriate bin in the histogram h by the floating-point
number weight."
  (gsl_histogram_accumulate (pointer h)
                            (coerce x 'double-float)
                            (coerce weight 'double-float))
  h)

(defun histogram-get (h i)
  "This function returns the contents of the i-th bin of the histogram
h."
  (gsl_histogram_get (pointer h) i))

(defun histogram-get-range (h i)
  "This function finds the upper and lower range limits of th i-th bin
of the histogram h. The lower limit is inclusive (i.e. events with
this coordinate are included in the bin) and the upper limit is
inclusibe (i.e. events with the coordinate of the upper limit are
excluded and fall in the neighboring higher bin, if it exists)."
  (cffi:with-foreign-objects ((lower :double) (upper :double))
    (gsl_histogram_get_range (pointer h) i lower upper)
    (values (cffi:mem-ref lower :double) (cffi:mem-ref upper :double))))

(defun histogram-max (h)
  "This function return the maximum upper of the histogram h."
  (gsl_histogram_max (pointer h)))

(defun histogram-min (h)
  "This function return the mimimum lower range of histogram h."
  (gsl_histogram_min (pointer h)))

(defun histogram-bins (h)
  "This function return the number of bins of the histogram h."
  (gsl_histogram_bins (pointer h)))

(defun histogram-reset (h)
  "This function resets all the bins in the histogram h to zero."
  (gsl_histogram_reset (pointer h)))


;;; searching histogram ranges

(defun histogram-find (h x)
  "This function finds and returns the index i to the bin number which
covers the coordinate x in the histogram h. The bin is located using a
binary search. The search includes an optimization for histograms with
uniform range, and will return the correct bin immediately in this
case."
  (cffi:with-foreign-object (i :unsigned-int)
    (gsl_histogram_find (pointer h) (coerce x 'double-float) i)
    (cffi:mem-ref i :unsigned-int)))


;;; histogram statistics

(defun histogram-max-val (h)
  "This function returns the maximum value contained in the histogram
bins"
  (gsl_histogram_max_val (pointer h)))

(defun histogram-max-bin (h)
  "This function returns the index of the bin containing the maximum
value. In the case where several bins contain the same maximum value
the smallest index is returned."
  (gsl_histogram_max_bin (pointer h)))

(defun histogram-min-val (h)
  "This function returns the minimum value contained in the histogram
bins"
  (gsl_histogram_min_val (pointer h)))

(defun histogram-min-bin (h)
  "This function returns the index of the bin containing the minimum
value. In the case where several bins contain the same minimum value
the smallest index is returned."
  (gsl_histogram_min_bin (pointer h)))

(defun histogram-mean (h)
  "This function returns the mean of the histogrammed variable, where
the histogram is regarded as a probability distribution. Negative bin
values are ignored for the purposes of this calculation. The accuracy
of the result is limited by the bin width."
  (gsl_histogram_mean (pointer h)))

(defun histogram-sigma (h)
  "This function returns the standard deviation of the histogrammed
variable, where the histogram is regarded as a probability
distribution. Negative bin values are ignored for the purposes of this
calculation. The accuracy of the result is limited by the bin width."
  (gsl_histogram_sigma (pointer h)))

(defun histogram-sum (h)
  "This function returns the sum of all bin values. Negative bin
values are included in the sum"
  (gsl_histogram_sum (pointer h)))


;;; histogram operations

(defun histogram-equal-bins-p (h1 h2)
  "This function returns t if the all of the individual bin ranges of
the two histograms are identical, and nil otherwise."
  (= (gsl_histogram_equal_bins_p (pointer h1) (pointer h2)) 1))

(defun histogram-add (h1 h2)
  "This function adds the contents of the bins in histogram h2 to the
corresponding bins of histogram h1, i.e. h'1(i) = h1(i) + h2(i). The
two histograms must have identical bin ranges."
  (gsl_histogram_add (pointer h1) (pointer h2))
  h1)

(defun histogram-sub (h1 h2)
  "This function subtracts the contents of the bins in histogram h2
from the corresponding bins of histogram h1, i.e. h'1(i) = h1(i) -
h2(i). The two histograms must have identical bin ranges."
  (gsl_histogram_sub (pointer h1) (pointer h2))
  h1)

(defun histogram-mul (h1 h2)
  "This function multiplies the contents of the bins of histogram h1
by the contents of the corresponding bins in histogram h2, i.e. h'1(i)
= h1(i) * h2(i). The tow histograms must have identical bin ranges."
  (gsl_histogram_mul (pointer h1) (pointer h2)))

(defun histogram-div (h1 h2)
  "This function divides the contents of the bins of histogram h1 by
contents of the corresponding bins in histogram h2, i.e. h'1(i) =
h1(i) / h2(i). The two histograms must have identical bin ranges."
  (gsl_histogram_div (pointer h1) (pointer h2)))

(defun histogram-scale (h scale)
  "This function multiplies the contents of the bins of histogram h by
the content scale, i.e. h'1(i) = h1(i) * scale."
  (gsl_histogram_scale (pointer h) (coerce scale 'double-float)))

(defun histogram-shift (h offset)
  "This function shifts the contents of the bins of histogram h by the
constant offset, i.e. h'1(i) = h1(i) + offset."
  (gsl_histogram_shift (pointer h) (coerce offset 'double-float)))


;;; the histogram probability distribution struct

(defun gsl_histogram_pdf_sample (p r)
  "This function uses r, a uniform random number between zero and one,
to compute a single random sample from the probability distribution
p. The algorithm used to compute the sample s is given by the
following formula,

  s = range[i] + delta ∗ (range[i + 1] − range[i])

where i is the index which satisfies sum[i] <= r < sum[i + 1] and
delta is (r − sum[i])/(sum[i + 1] − sum[i])."
  (gsl_histogram_pdf_sample (pointer p) (coerce r 'double-float)))
