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
    Svg: require('@site/static/img/undraw_docusaurus_mountain.svg').default,
    description: (
      <>
        Antithesis is a deterministic simulation testing platform that helps find bugs
        in distributed systems by exploring all possible execution paths. It provides
        comprehensive testing for complex blockchain systems like Cardano.
      </>
    ),
  },
  {
    title: 'Why use Anti-CLI?',
    Svg: require('@site/static/img/undraw_docusaurus_tree.svg').default,
    description: (
      <>
        Anti-CLI streamlines the process of submitting Cardano tests to Antithesis.
        It handles test orchestration, result collection, and provides a unified
        interface for managing your testing workflow with minimal setup.
      </>
    ),
  },
  {
    title: 'How it Works',
    Svg: require('@site/static/img/undraw_docusaurus_react.svg').default,
    description: (
      <>
        Anti-CLI operates through three main roles: Oracle validates and manages tests,
        Requester submits test configurations, and Agent executes tests and publishes
        results. This distributed architecture ensures scalable and reliable testing.
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
