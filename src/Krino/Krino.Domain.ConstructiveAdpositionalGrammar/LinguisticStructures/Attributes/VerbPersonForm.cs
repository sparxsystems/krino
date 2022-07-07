using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class VerbPersonForm : EnumGroupBase
    {
        public VerbPersonForm(EnumGroupBase parent) : base(parent)
        {
            FirstPerson = new EnumValue(this);
            SecondPerson = new EnumValue(this);
            ThirdPerson = new EnumValue(this);
        }

        public EnumValue FirstPerson { get; }

        public EnumValue SecondPerson { get; }

        public EnumValue ThirdPerson { get; }
    }
}
