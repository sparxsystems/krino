using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parser.Builder
{
    public interface IAdTreeBuilder
    {
        bool AddPattern(IPattern pattern);

        IEnumerable<IAdTree> ToAdTrees();
    }
}
