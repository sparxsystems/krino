using Krino.Vertical.Utils.Enums;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    public class VerbantLexemeAttributes : EnumGroupBase
    {
        public VerbantLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Verb = new VerbAttributes(this);
            Interjection = new EnumValue(this);
        }

        /// <summary>
        /// Verbant is a verb.
        /// </summary>
        public VerbAttributes Verb { get; }

        /// <summary>
        /// Verbant is an interjection.
        /// </summary>
        public EnumValue Interjection { get; }
    }
}
