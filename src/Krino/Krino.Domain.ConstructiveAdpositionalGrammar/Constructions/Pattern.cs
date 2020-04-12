using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Patterns;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {
        public IMorpheme Morpheme { get; set; }

        public int ValencyPosition { get; set; }

        public bool IsReversed { get; set; }

        public IRule<IPattern> AdPosition { get; set; } = It.IsAny<IPattern>();

        public IRule<IPattern> RightChild { get; set; } = It.IsAny<IPattern>();

        public IRule<IPattern> LeftChild { get; set; } = It.IsAny<IPattern>();

        public bool Equals(IPattern other)
        {
            throw new System.NotImplementedException();
        }
    }
}
