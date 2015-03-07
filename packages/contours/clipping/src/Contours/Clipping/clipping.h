/*
 *    Copyright (c) 2012. PARP Research Group.
 *    <http://perception.inf.um.es>
 *    University of Murcia, Spain.
 *
 *    This is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU Lesser General Public License as
 *    published by the Free Software Foundation, version 3 of the License.
 *
 *    It is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with QVision. If not, see <http://www.gnu.org/licenses/>.
 */

/// \file clipping.h
/// \brief Efficient set operations (union, intersection, diff, xor) for a pair of arbitrary closed polygons.
/// \author Copyright (C) PARP Research Group. University of Murcia, Spain.
///
/// This header file contains a C function which performs arbitrary clipping of two closed polygons, using the
/// technique explained in [1], as implemented by Adrián Amor Martínez & Pedro E. López-de-Teruel. See
/// documentation on that function for details on how you can use it from your code.
///
/// [1] G. Greiner, K. Hormann, "Efficient clipping of arbitrary polygons",
///     ACM Transactions on Graphics, vol. 17(2), April 1998.

#include <stdlib.h>

// Allowed clipping operations for clip function:
/// Clipping operation for INTERSECTION of polygons.
#define POLYGON_INTERSECTION 0x1
/// Clipping operation for UNION of polygons.
#define POLYGON_UNION        0x2
/// Clipping operation for DIFFERENCING of polygons A minus B.
#define POLYGON_DIFF_AB      0x4
/// Clipping operation for EXCLUSIVE OR (i.e. (A-B)U(B-A)) of polygons.
#define POLYGON_XOREXT       0x8
/// Clipping operation for DIFFERENCING of polygons B minus A.
#define POLYGON_DIFF_BA      0x16

// Possible output flags for clip function:
/// Input polygons do intersect in at least one vertex (in fact at least two vertices)
#define POLYGONS_INTERSECT   0
/// Input polygons do not intersect, and polygon A is inside polygon B.
#define A_INSIDE_B  1
/// Input polygons do not intersect, and polygon B is inside polygon A.
#define B_INSIDE_A  2
/// Input polygons do not intersect, and neither polygon A is inside polygon B, neither B is inside A.
#define DISJOINT_POLYGONS    3

// Possible marks for each output vertex:
/// Output vertex comes from polygon A
#define ORIGIN_A             1
/// Output vertex comes from polygon B
#define ORIGIN_B             2
/// Output vertex comes from an intersection, and next edge enters into polygon A
#define ORIGIN_INT_ENTERS_A  31
/// Output vertex comes from an intersection, and next edge enters into polygon B
#define ORIGIN_INT_ENTERS_B  32

/// @brief Function that performs any clippling operation (union, intersection, diff, xor) on a pair of input polygons.
///
/// Performs clipping of the polygon A with nA points against another polygon B with nB points. Returns a (possibly
/// empty) set of nl polygons with specified lengths in an array of coordinates polys. Some of the output polygons
/// can be termed as "negative" (for example, the XOR operation is called EXTENDED because it is returned with
/// sign, that is, A-B is returned as "positive" and B-A as"negative"; also, in a difference A-B when B is
/// completely contained by A, the output is given by two polygons, A and B, of which the first is output as
/// "positive", while the second is output as "negative").
///
/// Here is an example of output for the following two closed input polygons A={(0,0),(8,0),(8,7),(0,7),(0,0)},
/// and B={(5,-3),(5,2),(9,2),(7,4),(9,5),(-3,5),(5,-3)} (observe that input will only be correct if the last
/// vertex coincides with the first vertex on each polygon). See help on input/output parameters of the function
/// for further details on interpreting all the details of the example. The example is best interpreted if you
/// draw the polygons in a squared sheet.
///
/*!  @code
---------OPERATION: POLYGON_UNION---------
Output polygon 0:
--------------------------
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
x=5.00000, y=-3.0000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+0,+0)
x=2.00000, y=0.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.25000, indA=(+0,+1), alphaB=0.62500, indB=(+5,+2)
x=0.00000, y=0.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+0,+0), alphaB=0.00000, indB=(-1,-1)
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
x=-3.0000, y=5.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+5,+0)
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
x=0.00000, y=7.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+3,+0), alphaB=0.00000, indB=(-1,-1)
x=8.00000, y=7.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+2,+0), alphaB=0.00000, indB=(-1,-1)
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
x=9.00000, y=5.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+4,+0)
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_A, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
x=9.00000, y=2.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+2,+0)
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
x=8.00000, y=0.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+1,+0), alphaB=0.00000, indB=(-1,-1)
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
nlp=1 status=0
---------OPERATION: POLYGON_INTERSECTION---------
Output polygon 0:
--------------------------
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
x=5.00000, y=2.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+1,+0)
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
x=7.00000, y=4.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+3,+0)
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_A, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
x=2.00000, y=0.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.25000, indA=(+0,+1), alphaB=0.62500, indB=(+5,+2)
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
nlp=1 status=0
---------OPERATION: POLYGON_DIFF_AB---------
Output polygon 0:
--------------------------
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
x=5.00000, y=2.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+1,+0)
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
x=8.00000, y=0.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+1,+0), alphaB=0.00000, indB=(-1,-1)
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
Output polygon 1:
--------------------------
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
x=7.00000, y=4.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+3,+0)
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_A, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
Output polygon 2:
--------------------------
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
x=0.00000, y=7.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+3,+0), alphaB=0.00000, indB=(-1,-1)
x=8.00000, y=7.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+2,+0), alphaB=0.00000, indB=(-1,-1)
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
Output polygon 3:
--------------------------
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
x=2.00000, y=0.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.25000, indA=(+0,+1), alphaB=0.62500, indB=(+5,+2)
x=0.00000, y=0.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+0,+0), alphaB=0.00000, indB=(-1,-1)
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
nlp=4 status=0
---------OPERATION: POLYGON_DIFF_BA---------
Output polygon 0:
--------------------------
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
x=5.00000, y=-3.0000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+0,+0)
x=2.00000, y=0.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.25000, indA=(+0,+1), alphaB=0.62500, indB=(+5,+2)
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
Output polygon 1:
--------------------------
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
x=9.00000, y=2.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+2,+0)
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
Output polygon 2:
--------------------------
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_B, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
x=9.00000, y=5.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+4,+0)
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_B, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
Output polygon 3:
--------------------------
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
x=-3.0000, y=5.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+5,+0)
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
nlp=4 status=0
---------OPERATION: POLYGON_XOREXT---------
Output polygon 0:
--------------------------
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
x=5.00000, y=2.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+1,+0)
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
x=8.00000, y=0.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+1,+0), alphaB=0.00000, indB=(-1,-1)
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
Output polygon 1:
--------------------------
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
x=7.00000, y=4.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+3,+0)
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_A, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
Output polygon 2:
--------------------------
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
x=0.00000, y=7.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+3,+0), alphaB=0.00000, indB=(-1,-1)
x=8.00000, y=7.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+2,+0), alphaB=0.00000, indB=(-1,-1)
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
Output polygon 3:
--------------------------
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
x=2.00000, y=0.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.25000, indA=(+0,+1), alphaB=0.62500, indB=(+5,+2)
x=0.00000, y=0.00000, o=ORIGIN_A           , alphaA=0.00000, indA=(+0,+0), alphaB=0.00000, indB=(-1,-1)
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
Output polygon 4:
--------------------------
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
x=5.00000, y=-3.0000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+0,+0)
x=2.00000, y=0.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.25000, indA=(+0,+1), alphaB=0.62500, indB=(+5,+2)
x=5.00000, y=0.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.62500, indA=(+0,+2), alphaB=0.60000, indB=(+0,+1)
Output polygon 5:
--------------------------
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
x=9.00000, y=2.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+2,+0)
x=8.00000, y=3.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.42857, indA=(+1,+2), alphaB=0.50000, indB=(+2,+1)
x=8.00000, y=2.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+1,+1), alphaB=0.75000, indB=(+1,+1)
Output polygon 6:
--------------------------
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_B, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
x=9.00000, y=5.00000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+4,+0)
x=8.00000, y=5.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.71429, indA=(+1,+4), alphaB=0.08333, indB=(+4,+1)
x=8.00000, y=4.50000, o=ORIGIN_INT_ENTERS_B, alphaA=0.64286, indA=(+1,+3), alphaB=0.50000, indB=(+3,+1)
Output polygon 7:
--------------------------
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
x=-3.00000, y=5.0000, o=ORIGIN_B           , alphaA=0.00000, indA=(-1,-1), alphaB=0.00000, indB=(+5,+0)
x=0.00000, y=2.00000, o=ORIGIN_INT_ENTERS_A, alphaA=0.71429, indA=(+3,+2), alphaB=0.37500, indB=(+5,+1)
x=0.00000, y=5.00000, o=ORIGIN_INT_ENTERS_B, alphaA=0.28571, indA=(+3,+1), alphaB=0.75000, indB=(+4,+2)
nlp=4 status=0
@endcode
*/
///
/// @param Ax       (input) Array of X coordinates for vertices of polygon A.
/// @param Ay       (input) Array of Y coordinates for vertices of polygon A.
/// @param nA       (input) Number of vertices of polygon A.
/// @param Bx       (input) Array of X coordinates for vertices of polygon B.
/// @param By       (input) Array of Y coordinates for vertices of polygon B.
/// @param nB       (input) number of vertices of polygon B.
/// @param op       (input) One of the available set operations @ref POLYGON_INTERSECTION @ref POLYGON_UNION
///                         @ref POLYGON_DIFF_AB @ref POLYGON_XOREXT or @ref POLYGON_DIFF_BA
/// @param polysx  (output) Array of X coordinates for vertices of output polygons. Should be freed by caller
///                         after used. Its length is equal to the sum of integers contained in the output array
///                         \a lengths (see below).
/// @param polysy  (output) Array of Y coordinates for vertices of output polygons. Should be freed by caller
///                         after used. Its length is equal to the sum of integers contained in the output array
///                         \a lengths (see below).
/// @param lengths (output) Array of lengths (in number of vertices) of output polygons. Should be freed by caller
///                         after used.
/// @param nl      (output) Length of array \a lengths.
/// @param nlp     (output) Indicates how many of the output polygons are "positive". Positive polygons always come
///                         before negative ones in the output arrays (polysx,polysy,lengths, origin, etc.)
///                         You can always safely assume that \a nlp <= \a nl.
/// @param status  (output) Output status of the function. It can be any of the following: @ref POLYGONS_INTERSECT,
///                         @ref A_INSIDE_B, @ref B_INSIDE_A, or @ref DISJOINT_POLYGONS.
/// @param origin  (output) Array which indicates the origin (@ref ORIGIN_A, @ref ORIGIN_B,
///                         @ref ORIGIN_INT_ENTERS_A or @ref ORIGIN_INT_ENTERS_B) of each output vertex. Should be
///                         freed by caller after used. Its length is equal to the sum of integers contained in the
///                         output array \a lengths (i.e., the total number of output vertices, just as \a polysx
///                         and \a polysy, and also the following output arrays \a ind0A, \a ind1A, \a ind0B,
///                         \a ind1B, \a alphaA and \a alphaB, see below).
/// @param ind0A   (output) Array which indicates the "edge" in polygon A in which the corresponding output vertex
///                         is located. An "edge" starts in the corresponding input vertex, and ends just before
///                         the following input vertex (without including it). A value of -1 just indicates that
///                         the vertex is not contained into polygon A. This array should also be freed by
///                         caller after used, and its length is equal to the sum of integers contained in the
///                         output array lengths.
/// @param ind1A   (output) Array which indicates the vertex position in "edge" in polygon A in which the
///                         corresponding output vertex is located. 0 corresponds to the input vertex itself,
///                         while values >0 correspond to "pure intersection" output vertices, sorted along the
///                         edge. A value of -1 just indicates that the vertex is not contained into polygon
///                         A. Again, this array should also be freed by caller after used, and its length is
///                         equal to the sum of integers contained in the output array \a lengths.
/// @param ind0B   (output) Just the same as \a ind0A, but for vertices contained in polygon B.
/// @param ind1B   (output) Just the same as \a ind1A, but for vertices contained in polygon B.
/// @param alphaA  (output) Array with real values in the interval [0.0;1.0), indicating the exact position of the
///                         corresponding output vertices along the corresponding polygon A edges. A value equal to
///                         0.0 corresponds to first extreme of edge (input vertex), while 1.0 would correspond to
///                         last extreme of edge (i.e., next vertex in input polygon).
/// @param alphaB  (output) Just the same as \a alphaA, but for vertices contained in polygon B.
///
/// @returns 0 if success, 1 in case of failure.

int clip(double *Ax, double *Ay, int nA,
         double *Bx, double *By, int nB,
         int op,
         double **polysx, double **polysy,
         int **lengths, int *nl, int*nlp, int *status,
         int **origin,
          int **ind0A, int **ind1A, int **ind0B, int **ind1B,
         double **alphaA, double **alphaB);

