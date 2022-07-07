﻿using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IPhrase : IPhraseItem, ILinguisticStructure
    {
        List<IPhraseItem> DirectItems { get; }

        /// <summary>
        /// All items returned recursivelly.
        /// </summary>
        IEnumerable<IPhraseItem> AllItems { get; }
    }
}