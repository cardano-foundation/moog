import type {ReactNode} from 'react';
import clsx from 'clsx';
import Heading from '@theme/Heading';
import styles from './styles.module.css';

type FeatureItem = {
  title: string;
  Svg: React.ComponentType<React.ComponentProps<'svg'>>;
  description: ReactNode;
};

const FeatureList: FeatureItem[] = [
  {
    title: 'What is Antithesis?',
    Svg: require('@site/static/img/antithesis-mark.svg').default,
    description: (
      <>
         <a href="https://antithesis.com">Antithesis</a> is a <em>deterministic simulation testing</em> platform that helps find bugs
         in distributed systems by exploring multiple possible execution paths. It combines in a single integrated service:
         <ul>
           <li>a deterministic hypervisor able to run any kind of software compiled for a linux x86-64 machine,</li>
           <li>a framework to define and test properties over fuzzed execution of the <em>system-under-test</em>,</li>
           <li>a fault injection system aka. <em>chaos monkeys</em>,</li>
           <li>sophisticated reporting and debugging tools allowing time travel to help troubleshoot bugs.</li>
         </ul>
      </>
    ),
  },
  {
    title: 'Why use Moog?',
    Svg: require('@site/static/img/anticli.svg').default,
    description: (
        <>
            <strong>Moog</strong> aims at streamlining the process of orchestrating Antithesis tests for the Cardano community, providing an easy-to-use interface to the Antithesis service.
            Moog leverages the blockchain itself to track recording of users and projects, tests requests, and execution results thus providing auditability and traceability of the system usage.
            It demonstrates how Cardano can be used as an infrastructure to support its own development and the work of the Cardano community.
      </>
    ),
  },
  {
    title: 'How it Works',
    Svg: require('@site/static/img/how-it-works.svg').default,
    description: (
      <>
            <ol>
            <li><em>Users</em> register and submit test run requests through Moog, providing a <code>docker-compose</code> descriptor</li>
            <li>Requests are stored as transactions and validated by an <em>Agent</em></li>
            <li>The agent triggers a test run on <em>Antithesis</em> platform</li>
            <li>Test run results are made available through a secure link to Users</li>
        </ol>
        </>
    ),
  },
];

function Feature({title, Svg, description, imageLeft = true}: FeatureItem & {imageLeft?: boolean}) {
  return (
    <div className={clsx('col col--12')}>
      <div className="row" style={{alignItems: 'center', marginBottom: '2rem'}}>
        {imageLeft ? (
          <>
            <div className="col col--3">
              <div className="text--center">
                <Svg className={styles.featureSvg} role="img" />
              </div>
            </div>
            <div className="col col--9">
              <Heading as="h3">{title}</Heading>
              <p>{description}</p>
            </div>
          </>
        ) : (
          <>
            <div className="col col--9">
              <Heading as="h3">{title}</Heading>
              <p>{description}</p>
            </div>
            <div className="col col--3">
              <div className="text--center">
                <Svg className={styles.featureSvg} role="img" />
              </div>
            </div>
          </>
        )}
      </div>
    </div>
  );
}

export default function HomepageFeatures(): ReactNode {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} imageLeft={idx % 2 === 0} />
          ))}
        </div>
      </div>
    </section>
  );
}
