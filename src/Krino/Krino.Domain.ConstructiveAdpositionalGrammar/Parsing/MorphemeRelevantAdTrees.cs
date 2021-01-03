using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    /// <summary>
    /// Represents adtrees relevant for the morpheme.
    /// </summary>
    internal class MorphemeRelevantAdTrees
    {
        public List<IAdTree> AdTrees { get; private set; } = new List<IAdTree>();
    }
}
