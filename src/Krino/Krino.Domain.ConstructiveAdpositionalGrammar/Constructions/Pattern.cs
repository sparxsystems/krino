using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    public class Pattern : IPattern
    {
        public IMorpheme Morpheme { get; set; }

        public ulong PatternAttributes { get; set; }

        public IPattern RequiredAdPosition { get; set; }

        public IPattern RequiredLeft { get; set; }

        public IPattern RequiredRight { get; set; }

        public IPattern RequiredGovernor { get; set; }

        public int ValencyPosition => Attributes.PatternAttributes.ValencyPosition.GetValencyPosition(PatternAttributes);
    }
}
