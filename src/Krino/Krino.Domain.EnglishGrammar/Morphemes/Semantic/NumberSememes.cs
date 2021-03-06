﻿using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Semantic
{
    /// <summary>
    /// Number category.
    /// </summary>
    public class NumberSememes : EnumGroupBase
    {
        public NumberSememes(EnumGroupBase parent) : base(parent)
        {
            Singular = new EnumValue(this);
            Plural = new EnumValue(this);
        }

        public EnumValue Singular { get; }

        public EnumValue Plural { get; }
    }
}
