using Krino.Vertical.Utils.Enums;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Krino.Vertical.Utils_Tests.Enums
{
    [TestFixture]
    public class EnumerableExtTest
    {
        public class DummyEnumRoot : EnumRootBase
        {
            public class DummyCategory1 : EnumGroupBase
            {
                public class DummyCategory12 : EnumGroupBase
                {
                    public DummyCategory12(EnumGroupBase parent) : base(parent)
                    {
                        Attr111 = new EnumValue(this);
                        Attr112 = new EnumValue(this);
                        Attr113 = new EnumValue(this);
                    }

                    // 3rd bit ... BitIndex = 2
                    public EnumValue Attr111 { get; }
                    // 4th bit ... BitIndex = 3
                    public EnumValue Attr112 { get; }
                    // 5th bit ... BitIndex = 4
                    public EnumValue Attr113 { get; }
                }

                public DummyCategory1(EnumGroupBase parent) : base(parent)
                {
                    Category11 = new DummyCategory12(this);
                    Val12 = new EnumValue(this);
                }

                // 2nd bit ... BitIndex = 1
                public DummyCategory12 Category11 { get; }
                // 6th bit ... BitIndex = 5
                public EnumValue Val12 { get; }
            }



            public static DummyEnumRoot Instance { get; } = new DummyEnumRoot();

            // 1st bit ... BitIndex = 0
            public static DummyCategory1 Category1 { get; } = new DummyCategory1(Instance);
            // 7th bit ... BitIndex = 6
            public static EnumValue Val2 { get; } = new EnumValue(Instance);
        }


        [Test]
        public void AccumulateEnums()
        {
            var values = new BigInteger[] { DummyEnumRoot.Category1, DummyEnumRoot.Val2 };
            var result = values.AccumulateEnums();
            Assert.IsTrue(DummyEnumRoot.Category1.IsIn(result));
            Assert.IsTrue(DummyEnumRoot.Val2.IsIn(result));
        }
    }
}
