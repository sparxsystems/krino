using Krino.Vertical.Utils.Enums;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for words connecting clauses.
    /// </summary>
    public class MarkerWord : EnumGroupBase
    {
        public MarkerWord(EnumGroupBase parent) : base(parent)
        {
            IndependentMarkerWord = new EnumValue(this);
            DependentMarkerWord = new EnumValue(this);
        }

        /// <summary>
        /// A connecting word used at the beginning of an independent clause
        /// </summary>
        /// <remarks>
        /// An independent marker word is a connecting word used at the beginning of an independent clause. These words can always begin a sentence that can stand alone.
        /// E.g.: also, consequently, furthermore, however, moreover, nevertheless, therefore.
        /// </remarks>
        public EnumValue IndependentMarkerWord { get; }

        /// <summary>
        /// A connecting Word added to the beginning of an independent clause that makes it into a dependent clause.
        /// </summary>
        /// <remarks>
        /// E.g.: after, although, as, as if, because, before, even if, even though, if, in order to, since, though, unless, until, whatever, when, whenever, whether, while.
        /// </remarks>
        public EnumValue DependentMarkerWord { get; }
    }
}
