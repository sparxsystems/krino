using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class MorphemeRule
    {
        public string AcceptedMorph { get; set; }
        public string RejectedMorph { get; set; }

        public ulong AcceptedAttributes { get; set; }
        public ulong RejectedAttributes { get; set; }
    }
}
