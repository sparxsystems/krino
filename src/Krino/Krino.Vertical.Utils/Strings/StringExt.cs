using System;

namespace Krino.Vertical.Utils.Strings
{
    /// <summary>
    /// Utility functionality extending the string.
    /// </summary>
    public static class StringExt
    {
        /// <summary>
        /// Calculates Levenshtein distance between two strings.
        /// </summary>
        /// <remarks>
        /// The number of edits needed to turn one string into another.
        /// With Levenshtein distance, we measure similarity and match approximate strings with fuzzy logic.
        /// </remarks>
        /// <param name="src"></param>
        /// <param name="dest"></param>
        /// <returns></returns>
        public static int Distance(this string src, string dest)
        {
            int n = src.Length;
            int m = dest.Length;

            if (n == 0)
            {
                return m;
            }

            if (m == 0)
            {
                return n;
            }

            // Step 1
            // The matrix is initialized.
            int[,] d = new int[n + 1, m + 1];

            // Step 2
            // The matrix can be filled from the upper left to the lower right corner.
            for (int i = 0; i <= n; ++i)
            {
                d[i, 0] = i;
            }

            for (int j = 0; j <= m; ++j)
            {
                d[0, j] = j;
            }

            // Step 3
            // Each jump horizontally or vertically corresponds to an insert or a delete, respectively.
            for (int i = 1; i <= n; i++)
            {
                //Step 4
                // The cost is normally set to 1 for each of the operations.
                for (int j = 1; j <= m; j++)
                {
                    // Step 5
                    // The diagonal jump can cost either one, if the two characters in the row and column
                    // do not match else 0, if they match. Each cell always minimizes the cost locally.
                    int cost = (dest[j - 1] == src[i - 1]) ? 0 : 1;

                    // Step 6
                    // This way the number in the lower right corner is the Levenshtein distance between both words.
                    d[i, j] = Math.Min(
                        Math.Min(
                        d[i - 1, j] + 1,            // deletion
                        d[i, j - 1] + 1),           // insertion
                        d[i - 1, j - 1] + cost);    // substitution
                }
            }

            return d[n, m];
        }
    }
}
