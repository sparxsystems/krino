﻿using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Text : LinguisticStructureBase, IText
    {
        public Text(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<ISentence> Sentences { get; } = new List<ISentence>();

        public string Value => string.Join(" ", Sentences.Select(x => x.Value)).Trim();

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", Sentences.Select(x => x.GrammarStr)), ")");

        public ILinguisticStructure DeepCopy()
        {
            var sentences = Sentences.Select(x => x.DeepCopy()).OfType<ISentence>();
            var result = new Text(Attributes);
            result.Sentences.AddRange(sentences);
            return result;
        }
    }
}
