﻿using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal abstract class TermBase : LinguisticStructureBase, ITerm
    {
        public TermBase(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<IWord> Words { get; } = new List<IWord>();

        public string Value => string.Join(" ", Words.Select(x => x.Value));
    }
}
