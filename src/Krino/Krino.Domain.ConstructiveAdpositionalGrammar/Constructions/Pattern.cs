using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Patterns;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {
        public IMorpheme Morpheme { get; set; }

        public int ValencyPosition { get; set; }

        public bool IsCorrelativeAdposition { get; set; }

        public bool IsCorrelativeSubstitute { get; set; }

        public IRule<IPattern> AdPosition { get; set; } = It.IsAny<IPattern>();

        public IRule<IPattern> Right { get; set; } = It.IsAny<IPattern>();

        public IRule<IPattern> Left { get; set; } = It.IsAny<IPattern>();

        public bool Equals(IPattern other)
        {
            throw new System.NotImplementedException();
        }
    }
}
