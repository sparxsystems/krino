using Krino.Vertical.Utils.Collections;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    public class AdTree : TreeComposite<IAdTree>,  IAdTree
    {
        public AdTree(IAdTreeItem item)
        {
        }

    }
}
