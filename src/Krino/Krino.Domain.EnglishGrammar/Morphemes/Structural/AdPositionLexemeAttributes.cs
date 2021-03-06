﻿using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for the adposition lexemes.
    /// </summary>
    public class AdPositionLexemeAttributes : EnumGroupBase
    {
        public AdPositionLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Conjunction = new ConjunctionAttributes(this);
        }

        /// <summary>
        /// AdPosition is a conjunction.
        /// </summary>
        public ConjunctionAttributes Conjunction { get; }
    }
}
