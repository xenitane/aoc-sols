struct Component {
    ports: [u32; 2],
}
impl Component {
    fn from(p0: u32, p1: u32) -> Component {
        Component { ports: [p0, p1] }
    }
    fn can_connect(&self, pin: u32) -> bool {
        return pin == self.ports[0] || pin == self.ports[1];
    }
}

struct Bridge {
    current: u32,
    length: u32,
    strength: u32,
    max_strength: u32,
    max_len_strength: (u32, u32),
}
impl Bridge {
    fn connect(&mut self, component: &Component) {
        self.strength += component.ports[0] + component.ports[1];
        self.length += 1;

        self.max_strength = self.max_strength.max(self.strength);

        if self.length > self.max_len_strength.0 {
            self.max_len_strength.0 = self.length;
        }
        if self.length == self.max_len_strength.0 {
            self.max_len_strength.1 = self.max_len_strength.1.max(self.strength);
        }

        self.current = if component.ports[0] == self.current {
            component.ports[1]
        } else if component.ports[1] == self.current {
            component.ports[0]
        } else {
            unreachable!()
        };
    }
    fn disconnect(&mut self, component: &Component) {
        self.strength -= component.ports[0] + component.ports[1];
        self.length -= 1;
        self.current = if component.ports[0] == self.current {
            component.ports[1]
        } else if component.ports[1] == self.current {
            component.ports[0]
        } else {
            unreachable!()
        };
    }
}

fn maximize_bridge_strength(components: &mut Vec<(Component, bool)>, bridge: &mut Bridge) {
    let len = components.len();
    for i in 0..len {
        if !components[i].1 && components[i].0.can_connect(bridge.current) {
            bridge.connect(&components[i].0);
            components[i].1 = true;
            maximize_bridge_strength(components, bridge);
            bridge.disconnect(&components[i].0);
            components[i].1 = false;
        }
    }
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let mut components: Vec<_> = input
        .lines()
        .map(|line| {
            let pins: Vec<_> = line.split('/').map(|v| v.parse::<u32>().unwrap()).collect();
            (Component::from(pins[0], pins[1]), false)
        })
        .collect();

    let mut bridge = Bridge {
        current: 0,
        length: 0,
        strength: 0,
        max_strength: 0,
        max_len_strength: (0, 0),
    };

    maximize_bridge_strength(&mut components, &mut bridge);

    let first = bridge.max_strength;
    let second = bridge.max_len_strength.1;

    Ok((first, second))
}
