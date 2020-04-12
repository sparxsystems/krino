﻿using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries
{
    public class ConstructiveDictionary : IConstructiveDictionary
    {
        public List<IPattern> Patterns { get; set; } = new List<IPattern>();

        public MultiDictionary<string, IMorpheme> Lexemes { get; set; } = new MultiDictionary<string, IMorpheme>();
    }
}
