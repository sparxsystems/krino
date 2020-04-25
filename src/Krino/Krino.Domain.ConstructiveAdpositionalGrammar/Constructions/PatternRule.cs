using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class PatternRule
    {
        public MorphemeRule MorphemeRule { get; set; }

        public ulong AcceptedAttribute { get; set; }
        public ulong RejectedAttribute { get; set; }
    }
}
