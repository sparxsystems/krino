using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class ComplexSentenceAttributes : EnumGroupBase
    {
        public ComplexSentenceAttributes(EnumGroupBase parent) : base(parent)
        {
            Argument = new EnumValue(this);
            Condition = new EnumValue(this);
        }

        public EnumValue Argument { get; }

        public EnumValue Condition { get; }
    }
}
