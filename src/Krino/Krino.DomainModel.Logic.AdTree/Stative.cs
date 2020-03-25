using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.DomainModel.Logic.AdTree
{
    public class Stative : GrammarCharacter
    {
        /// <summary>
        /// Indicates which valency is saturated by this stative.
        /// </summary>
        public int ValencyIdx { get; set; }
    }
}
