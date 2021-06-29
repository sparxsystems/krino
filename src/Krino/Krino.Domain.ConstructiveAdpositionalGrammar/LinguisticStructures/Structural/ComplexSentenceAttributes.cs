using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class ComplexSentenceAttributes : EnumGroupBase
    {
        public ComplexSentenceAttributes(EnumGroupBase parent) : base(parent)
        {
            Argument = new EnumValue(this);
        }

        public EnumValue Argument { get; }
    }
}
